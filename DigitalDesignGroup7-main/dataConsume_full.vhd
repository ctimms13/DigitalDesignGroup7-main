library ieee;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned."*";
use ieee.std_logic_unsigned."+";
use ieee.std_logic_unsigned."-";
use ieee.std_logic_unsigned."=";
use work.common_pack.all;

entity dataConsume is
	port (
	  ----    GLOBAL     ----
	  clk:		in std_logic;                                      -- clock
		reset:		in std_logic;                                    -- synchronous reset
		
		----  CMD to DATA  ----
		start: in std_logic;                                     -- goes high to signal data transfer
		numWords_bcd: in BCD_ARRAY_TYPE(2 downto 0);             -- how many words we should fetch from the generator
		
		----  DATA to CMD  ----
		dataReady: out std_logic;                                -- telling the cmdProcessor that our data is ready
		byte: out std_logic_vector(7 downto 0);                  -- the latest byte from the generator gets propagated through
		seqDone: out std_logic;                                  -- telling the cmdProcessor that all the data has been processed
		maxIndex: out BCD_ARRAY_TYPE(2 downto 0);                -- the index of the maximum value
		dataResults: out CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);-- 7 bytes with the biggest in the middle
		
		---- DATA <-> GEN  ----
		ctrlIn: in std_logic;                                    -- data from the processor is valid
		ctrlOut: out std_logic;                                  -- request data from the processor
		data: in std_logic_vector(7 downto 0)                    -- the data from the processor
	);
end dataConsume;

architecture behav of dataConsume is
  type state_type is (INITIAL,INIT,REQ_DATA,WAIT_DATA,DATA_VALID,SEQ_DONE);
  signal curState, nextState: state_type;
  signal numWords_bin, loopIndex_bin: std_logic_vector(10 downto 0);
  signal curMax: std_logic_vector(7 downto 0);
  signal curDataResults,curMaxDataResults: CHAR_ARRAY_TYPE(0 to 6);
  signal ctrlIn_delayed, ctrlIn_detected, ctrlOut_reg: std_logic;
  signal curMaxIndex, loopIndex: BCD_ARRAY_TYPE(2 downto 0);
  signal last3: CHAR_ARRAY_TYPE(0 to 2);
  signal toggle,toggle1,count1,count2,count3,first: std_logic;
  begin
    ------------------------------------- Delay the ctrlIn to moniter any edge rising or falling
    delay_CtrlIn: process(clk)     
    begin
      if rising_edge(clk) then
        ctrlIn_delayed <= ctrlIn;
     end if;
    end process;
  
    ctrlIn_detected <= ctrlIn xor ctrlIn_delayed;
    ------------------------------------- Logic for moving between states and processing the data
    combi_state : process (curState, start, ctrlIn_detected, ctrlIn)
    begin
      case curState is
    --------------------------- Inital state, just waits for the start signal, sets default vales when we start and moves on to REQ_DATA state
       when INITIAL =>
            loopIndex <= (others=>"0000");
            loopIndex_bin <= "00000000000";
            curDataResults <= (others=>"00000000");
            count1 <= '0';
            count2 <= '0';
            count3 <= '0';
            curMax <= "00000000";
            curMaxDataResults <= (others=> "00000000");
            toggle <= '1';
            toggle1 <= '0';
            seqDone <= '0';
            dataReady <= '0';
            maxIndex <= (others => "1111");
            byte <= data;
            ctrlOut <= '0';
            nextState<= INIT;
            
        -----------------------------------------
       when INIT =>   
         toggle1 <= '0';
         numWords_bin <= (numWords_bcd(2)*"1100100") + (numWords_bcd(1)*"1010") + (numWords_bcd(0));                
         IF (start = '1')AND (toggle1 = '0') THEN
           toggle1 <= '1';
           nextState <= REQ_DATA;
         ELSE 
          nextState <= INIT;
        END IF; 
        dataResults <= curMaxDataResults;
        maxIndex <= curMaxIndex;
    ---------------------------- Request data by sending ctrlOut, update our outputs with the current local versions
      when REQ_DATA =>   
        ctrlOut <= toggle;
        first <= '0';
        toggle <= NOT toggle after 1 ns;
        nextState <= WAIT_DATA;
        --dataResults <= curMaxDataResults;
        --maxIndex <= curMaxIndex;
                                      
    --------------------------- Wait for the data from the generator, loop here until we get it
      when WAIT_DATA =>
        if ctrlIn_detected <= '0'  then
          nextState <= DATA_VALID;
        else
          nextState <= WAIT_DATA;
       end if;
    --------------------------- Performs comparisons to detect a peak and keeps track of the index
      when DATA_VALID => 
        dataReady <= '1';             
        if loopIndex_bin + "00000000001" < numWords_bin then
          if loopIndex(1) = "1001" AND loopIndex(0) = "1001" then
              loopIndex(2) <= (loopIndex(2) + "0001");
              loopIndex(1) <= "0000";
              loopIndex(0) <= "0000";
          elsif loopIndex(0) = "1001" then
              loopIndex(1) <= loopIndex(1) + "0001";
              loopIndex(0) <= "0000";
          else
              loopIndex(0) <= loopIndex(0) + "0001";
          end if;
          curDataResults(0 to 3) <= curDataResults(1 to 3) & data;
          loopIndex_bin <= loopIndex_bin + 1;
          nextState <= INIT;
        else          
           nextState <= SEQ_DONE;
           dataReady <= '1';          
        end if;
        -----------------------    If a peak is detected update max registers and process dataResults
        if data > curMax AND data /= "11011101" then 
           count1 <= '1';
           curMax <= data;
           curMaxIndex <= loopIndex;
           curMaxIndex(0) <= loopIndex(0);        
        end if;
        -----------------------   
        if curDataResults(3) > curMaxDataResults(3) and curDataResults(3) /= "11011101" then
          curMaxDataResults <= curDataResults;
          count1 <= '1';
        end if;
        -----------------------  Counting logic to update dataResults
        if count3 = '1' then
          curMaxDataResults(6) <= data;
          count3 <= '0';
        end if;
        if count2 = '1' then
          curMaxDataResults(5) <= data;
          count3 <= '1';
          count2 <= '0';
        end if;
        if count1 = '1' then
          curMaxDataResults(4) <= data;
          curMaxDataResults(6) <= "00000000";
          count2 <= '1';
          count1 <= '0';
        end if;        
        byte <= data;               
    ------------------------ State for the end of operation, send seqDone and reset
      when SEQ_DONE =>  
        dataResults <= curMaxDataResults; 
        seqDone <= '1';
        dataReady <= '0';
        first <= '1';
        nextState <= INITIAL;
       end case;
    end process;
    --------------------------------------- Process next state based on the clock timer
    seq_state: process (clk, reset)
    begin
      if reset = '1' then
        curState <= INITIAL;
      elsif clk'EVENT AND clk='1' then
        curState <= nextState;
     end if;
    end process;
  end;

library ieee;
use ieee.std_logic_1164.all;
use work.common_pack.all;
--USE ieee.std_logic_arith.all;
use ieee.numeric_std.all;       
entity cmdProc is
	port (
	  --Outside porty
	    clk:		in std_logic;
      reset:		in std_logic;
    --Rx to Cmd ports  
      rxNow:		in std_logic; --Valid
      rxData:			in std_logic_vector (7 downto 0);
      rxDone:		out std_logic; --done
      ovErr:		in std_logic;
      framErr:	in std_logic;
    --Tx to Cmd ports  
      txnow:		out std_logic;
      txdone:		in std_logic;
      txData:			out std_logic_vector (7 downto 0);
      -- CMD to DATA
      start: out std_logic;
      numWords_bcd: out BCD_ARRAY_TYPE(2 downto 0);
      dataReady: in std_logic;
      byte: in std_logic_vector(7 downto 0);
      maxIndex: in BCD_ARRAY_TYPE(2 downto 0);
      dataResults: in CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
      seqDone: in std_logic
	 );
	end;
	--C:/Users/lg20264/OneDrive - University of Bristol/Documents/Assignment 2/Project/cmdProc.vhd
	ARCHITECTURE behav OF cmdProc IS
  -- State declaration
  TYPE state_type IS (
      INITIAL,
			IDLE,
			RCV_SERIAL,
			START_ECHO_CMD,
			ECHO_CMD,
			PROCESS_BYTE,			
			REQ_DATA,
			BEGIN_DATA,
			RCV_DATA,
			START_DATA_TX,
			DATA_TX,
			REQ_RESULT,
			RESET_TX,
			REQ_CTRL,
			CHECK_OUTPUT_TYPE,
			GET_SEQ,
			GET_PEAK,
			HEX_DATA,
			HEX_DATA2,
			HEX_DATA3,
		PRINT_PEAK);  -- List your states here 	
  SIGNAL curState, nextState: state_type;
  signal data_rec, echo, data_send, out_val: std_logic_vector (7 downto 0);
  signal max_out_bcd: BCD_ARRAY_TYPE(2 downto 0);
  signal valid_count: integer;
  signal out_full_res: CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1);
  signal value: STD_LOGIC_VECTOR(3 DOWNTO 0);
  type asciiArray is ARRAY(2 DOWNTO 0) of STD_LOGIC_VECTOR(7 DOWNTO 0);
	signal ascii_out: asciiArray;
	
	
	BEGIN
	  
	combi_nextState: PROCESS(curState, rxNow, ovErr, framErr, txDone, dataReady, seqDone, data_rec, valid_count, max_out_bcd, out_val)
	variable byte_1, byte_2, byte_3: STD_LOGIC_VECTOR(3 DOWNTO 0); 
	variable result_type: STD_LOGIC_VECTOR(7 DOWNTO 0);
	variable counter, data_done, bandage: STD_LOGIC;
	variable ascii_count: unsigned(2 downto 0);
	variable ascii_count_int: integer;
	
	
  BEGIN
    CASE curState IS
      
      WHEN INITIAL =>
      --initalise all variables 
        data_rec <= "00000000";
        echo <= "00000000";
        data_send <= "00000000";
        max_out_bcd <= (others => "0000");
        out_full_res <= (others => "00000000");
        value <= "0000";
        ascii_out <= (others => "00000000");
        byte_1 := "0000";
        byte_2 := "0000";
        byte_3 := "0000";
        result_type := "00000000";
        counter := '0';
        data_done := '0';
        ascii_count := "000";
        valid_count <= 0;
        start <= '0';
        nextState <= RCV_SERIAL;
        rxDone <= '0';
        txNow <= '0';
        txData <= "00000000";
        bandage := '0';
        
      WHEN IDLE =>
	      counter := '0';
        rxDone <= '0';
        if rxNow = '1' then
          nextState <= RCV_SERIAL;
        else
          nextState <= IDLE;
        end if;
          
        
      WHEN RCV_SERIAL =>
        --Recieve the byte
        data_rec <= rxData;
        nextState <= START_ECHO_CMD;
        rxDone <= '1';

     WHEN START_ECHO_CMD =>
      rxDone <= '0';
       IF ovErr = '1' or framErr = '1' THEN
         nextState <= IDLE;
         --RCVSERIAL
       ELSE
         echo <= data_rec;
         nextState <= ECHO_CMD;
       END IF;  
       
     WHEN ECHO_CMD =>
       IF txDone = '1' THEN
         txNow <= '1';
         txData <= echo; --ECHO IS ALREADY IN ASCII FORMAT
         nextState <= PROCESS_BYTE;
       ELSE 
         txNow <= '0';
         nextState <= ECHO_CMD;
       END IF;
       
     WHEN PROCESS_BYTE =>
        txNow <= '0';
        CASE valid_count IS
        WHEN 0 => 
          if (to_integer(unsigned(data_rec)) = 65 or to_integer(unsigned(data_rec)) =97) and counter = '0' then 
            valid_count <= 1; 
            counter := '1';
          end if;
          nextState <= 	IDLE;
        WHEN 1 => 
          if (to_integer(unsigned(data_rec)) >= 48 and to_integer(unsigned(data_rec)) <= 57) and counter = '0' then 
            byte_1 := data_rec(3 DOWNTO 0);
            valid_count <= 2;
            counter := '1';
          elsif (counter = '0') then 
            valid_count <= 0; 
          end if;
          nextState <= 	IDLE;
        WHEN 2 =>  
          if (to_integer(unsigned(data_rec)) >= 48 and to_integer(unsigned(data_rec)) <= 57) and counter = '0' then
            byte_2 := data_rec(3 DOWNTO 0);
            valid_count <= 3;
            counter := '1';
          elsif (counter = '0') then 
            valid_count <= 0; 
          end if;
          nextState <= 	IDLE;
        WHEN 3 => 
          if (to_integer(unsigned(data_rec)) >= 48 and to_integer(unsigned(data_rec)) <= 57) and counter = '0' then
            byte_3 := data_rec(3 DOWNTO 0);
            nextState <= REQ_DATA;
          elsif (counter = '0') then 
            valid_count <= 0;
            nextState <= IDLE; 
          end if;
        WHEN OTHERS => 
          valid_count <= 41;
          nextState <= IDLE;
        END CASE;
        
     WHEN REQ_DATA =>
       numWords_bcd(2) <= byte_1;
       numWords_bcd(1) <= byte_2;
       numWords_bcd(0) <= byte_3;
       nextState <= BEGIN_DATA;
       
     WHEN BEGIN_DATA =>
       start <= '1';
       nextState <= RCV_DATA;
       
     WHEN RCV_DATA =>
       start <= '0';
       IF dataReady = '1' THEN
         --start <= '0';
         data_send <= byte;
         nextState <= START_DATA_TX;
       ELSE
         nextState <= RCV_DATA;
       END IF;
       
    WHEN START_DATA_TX =>
      --data_send <= byte;
       IF seqDone = '1' AND bandage = '0' THEN
         data_done := '1';
         out_full_res <= dataResults;
         max_out_bcd <= maxIndex;
        
       END IF;
       IF txDone = '1' THEN
         txNow <= '1'; 
         --txData <= data_send;
         nextState <= RESET_TX;
         value <= data_send(7 downto 4);
         txData <= out_val;
       ELSE
         nextState <= START_DATA_TX;
       END IF;
       
     WHEN RESET_TX =>
       txNow <= '0';
       nextState <= DATA_TX;
       
       
    WHEN DATA_TX =>
      IF txDone = '1' THEN
         txNow <= '1'; 
         nextState <= REQ_RESULT;
         value <= data_send(3 downto 0);
         txData <= out_val;
      ELSE 
         nextState <= DATA_TX;
      END IF;
       
       
     WHEN REQ_RESULT =>
       txNow <= '0';
       IF (data_done = '1' AND bandage = '1') THEN
         nextState <= HEX_DATA;
       ELSIF (data_done = '1' AND bandage = '0') THEN
         nextState <= RCV_DATA;
         bandage := '1';
       ELSE
         data_done := '0';
         nextState <= BEGIN_DATA;
       END IF;
       txData <= "00100000";
       

     WHEN HEX_DATA =>
        value <= max_out_bcd(0);
        ascii_out(0) <= out_val; 
        nextState <= HEX_DATA2;
        
      WHEN HEX_DATA2 =>
        value <= max_out_bcd(1);
        ascii_out(1) <= out_val;
        nextState <= HEX_DATA3;
          
      WHEN HEX_DATA3 =>
         value <= max_out_bcd(2);
        ascii_out(2) <= out_val;
        --ascii_count := "010";
        ascii_count_int := 2; 
        nextState <= REQ_CTRL;
        
      WHEN REQ_CTRL =>
       if txDone = '1' then
         txNow <= '1';
         --ascii_count := ascii_count - "001";
         --txData <= ascii_out(to_integer(unsigned(ascii_count))+1);
         --txData <= ascii_out(ascii_count_int);
         --ascii_count_int := ascii_count_int - 1;
         txData <= ascii_out(ascii_count_int);
         nextState <= PRINT_PEAK;
       else
         nextState <= REQ_CTRL;
       end if;
     
     WHEN PRINT_PEAK =>
        txNow <= '0';
        --txData <= ascii_out(to_integer(ascii_count)-1);
        if ascii_count_int > 0 then
          ascii_count_int := ascii_count_int - 1;
          nextState <= REQ_CTRL;
        else 
          nextState <= INITIAL; 
        end if;
    WHEN OTHERS =>
        nextState <= IDLE;

    END CASE;
  END PROCESS;
	  
	  
	  lookup_hex: PROCESS(value)
	   BEGIN
	     IF curState = INITIAL THEN
	       out_val <= "00000000";
	     ELSE
	      CASE value IS
	      WHEN "0000" => 
	        out_val <= "00110000"; --0
	      WHEN "0001" => 
	        out_val <= "00110001"; --1
	      WHEN "0010" => 
	        out_val <= "00110010"; --2
	      WHEN "0011" => 
	        out_val <= "00110011"; --3
	      WHEN "0100" => 
	        out_val <= "00110100"; --4
	      WHEN "0101" => 
	        out_val <= "00110101"; --5
	      WHEN "0110" => 
	        out_val <= "00110110"; --6
	      WHEN "0111" => 
	        out_val <= "00110111"; --7
	      WHEN "1000" => 
	        out_val <= "00111000"; --8
	      WHEN "1001" => 
	        out_val <= "00111001"; --9
	      WHEN "1010" => 
	        out_val <= "01000001"; --A
	      WHEN "1011" => 
	        out_val <= "01000010"; --B
	      WHEN "1100" => 
	        out_val <= "01000011"; --C
	      WHEN "1101" => 
	        out_val <= "01000100"; --D
	      WHEN "1110" => 
	        out_val <= "01000101"; --E
	      WHEN "1111" => 
	        out_val <= "01000110"; --F
	      WHEN OTHERS => 
	        out_val <= "11111111"; --CATCH
	      END CASE;
	     END IF;
	    END PROCESS;
	  
	  --Sequantial logic for changing state
	seq_state: PROCESS (clk, reset)
  BEGIN
    IF reset = '1' THEN
      curState <= INITIAL;
    ELSIF clk'EVENT AND clk='1' THEN
      curState <= nextState;
    END IF;
  END PROCESS; -- seq
	
end behav;


-- Entity: handshake
-- Description: This entity implements a handshake protocol to ensure (with
-- high probability) that data being transferred across clock domains is
-- stable. Note that this handshake has an additional control signal
-- (delay_ack), which allows the dest domain to delay acknowledgements to the
-- source domain. This signal is useful when the dest domain is not prepared to
-- accept new data.

library ieee;
use ieee.std_logic_1164.all;

entity handshake is
    port (
        clk_src   : in  std_logic;
        clk_dest  : in  std_logic;
        rst       : in  std_logic;
        go        : in  std_logic;
        delay_ack : in  std_logic;
        rcv       : out std_logic;
        ack       : out std_logic
        );
end handshake;

-- TODO: The following implementation of the handshake simulates perfectly, but
-- will not work on the FPGA. You must fix it.

architecture TRANSITIONAL of handshake is

    type state_type is (S_READY, S_WAIT_FOR_ACK, S_RESET_ACK);
    type state_type2 is (S_READY, S_SEND_ACK, S_RESET_ACK);
    signal state_src  : state_type;
    signal state_dest : state_type2;

    signal send_src_r : std_logic;
    signal ack_dest_r : std_logic;
    
    -- added signals
    signal ack_dest_sync_0, ack_dest_sync_1 : std_logic;
    signal send_src_sync_0, send_src_sync_1 : std_logic;
begin

    -----------------------------------------------------------------------------
    -- State machine in source domain that sends to dest domain and then waits
    -- for an ack

    process(clk_src, rst)
    begin
        if (rst = '1') then
            state_src  <= S_READY;
            send_src_r <= '0';
            ack        <= '0';
            ack_dest_sync_0 <= '0';
            ack_dest_sync_1 <= '0';
        elsif (rising_edge(clk_src)) then

            ack <= '0';
            ack_dest_sync_0 <= ack_dest_r;
            ack_dest_sync_1 <= ack_dest_sync_0;

            case state_src is
                when S_READY =>
                    if (go = '1') then
                        send_src_r <= '1';
                        state_src  <= S_WAIT_FOR_ACK;
                    end if;

                when S_WAIT_FOR_ACK =>
                    if (ack_dest_sync_1 = '1') then
                        send_src_r <= '0';
                        state_src  <= S_RESET_ACK;
                    end if;

                when S_RESET_ACK =>
                    if (ack_dest_sync_1 = '0') then
                        ack       <= '1';
                        state_src <= S_READY;
                    end if;

                when others => null;
            end case;
        end if;
    end process;

    -----------------------------------------------------------------------------
    -- State machine in dest domain that waits for source domain to send signal,
    -- which then gets acknowledged

    process(clk_dest, rst)
    begin
        if (rst = '1') then
            state_dest <= S_READY;
            ack_dest_r <= '0';
            rcv        <= '0';
            send_src_sync_0 <= '0';
            send_src_sync_1 <= '0';
        elsif (rising_edge(clk_dest)) then

            rcv <= '0';
            send_src_sync_0 <= send_src_r;
            send_src_sync_1 <= send_src_sync_0;
            
            case state_dest is
                when S_READY =>
                    -- if source is sending data, assert rcv (received)
                    if (send_src_sync_1 = '1') then
                        rcv        <= '1';
                        state_dest <= S_SEND_ACK;
                    end if;

                when S_SEND_ACK =>
                    -- send ack unless it is delayed
                    if (delay_ack = '0') then
                        ack_dest_r <= '1';
                        state_dest <= S_RESET_ACK;
                    end if;

                when S_RESET_ACK =>
                    -- send ack unless it is delayed
                    if (send_src_sync_1 = '0') then
                        ack_dest_r <= '0';
                        state_dest <= S_READY;
                    end if;

                when others => null;
            end case;
        end if;
    end process;

end TRANSITIONAL;

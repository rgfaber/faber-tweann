#!/usr/bin/env bash
# Persistent launcher for EXP_023 on msi00. Run detached via:
#   setsid ~/run_exp023_msi00.sh < /dev/null > /dev/null 2>&1 &
# Results stream to /tmp/exp023_results.txt; full log to ~/exp023.log.
set -u
cd "$HOME/faber-tweann" || exit 1
# shellcheck disable=SC1091
. "$HOME/.asdf/asdf.sh"
export PATH="$HOME/bin:$HOME/.cargo/bin:$PATH"
echo "EXP_023 started $(date -u +%FT%TZ) on $(erl -eval 'io:format("OTP ~s",[erlang:system_info(otp_release)]),halt().' -noshell)" > "$HOME/exp023.log"
rebar3 eunit --module=exp023_tests >> "$HOME/exp023.log" 2>&1
echo "EXP_023 finished $(date -u +%FT%TZ)" >> "$HOME/exp023.log"

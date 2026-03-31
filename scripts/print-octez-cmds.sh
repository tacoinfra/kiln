set -euo pipefail

# Read CMD_NAME environment variable or use 'node' by default.
name="${CMD_NAME:-node}"

# Match either 'tezos' or 'octez' prefixes.
pattern="(^|[ /])(tezos-${name}|octez-${name})([ ]|$)"

# Check if `pgrep` command exists. If it does obtain the process id
# of the process using it. If it does not use `ps` command.
if command -v pgrep >/dev/null 2>&1; then
  pids=$(pgrep -f "tezos-$name"; pgrep -f "octez-$name")

  if [[ -z "$pids" ]]; then
    pids=$(pgrep -f "$pattern" || true)
  fi
else
  pids=$(ps ax -o pid= -o command= \
    | grep -E "$pattern" \
    | grep -v grep \
    | awk '{print $1}')
fi

# If no process id was obtained, then script failed.
if [[ -z "$pids" ]]; then
  echo "No process found for: $name" >&2
fi

# Read the first process id to pid variable
IFS= read -r pid <<< "$pids"

# Initialize an empty string variable that will be assigned the current working
# directory of the process.
cwd=""

# Check if /proc/$pid exists, if it does read it into cwd
if [[ -d "/proc/$pid" ]]; then
  cwd=$(readlink -f "/proc/$pid/cwd" 2>/dev/null || true)
fi

# Initialize an empty string variable that will be assigned the command line
# arguments that was used to start the process.
cmd=""

# Check if /proc/$pid/cmdline exists, and if it does, read the lines and append to `cmd` variable
if [[ -r "/proc/$pid/cmdline" ]]; then
  while IFS= read -r -d '' arg; do
    cmd+="$(printf '%q ' "$arg")"
  done < "/proc/$pid/cmdline"
fi

# If cmd is empty, then the script failed.
[[ -z "$cmd" ]] && cmd="(failed to extract argv)"

# If cwd and cmd is non empty make the command to change dir to cwd and execute the command.
# Or else just print the command.
if [[ -n "$cwd" ]]; then
  printf 'cd %q && %s\n\n' "$cwd" "$cmd"
else
  printf '%s\n\n' "$cmd"
fi

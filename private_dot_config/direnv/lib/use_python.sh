use_python() {
	VIRTUAL_ENV="$(expand_path .venv)"
	VIRTUAL_ENV_PROMPT="(.venv) "
	IPYTHONDIR="$(expand_path .ipython)"
	PS1="${VIRTUAL_ENV_PROMPT}${PS1}"
	export VIRTUAL_ENV IPYTHONDIR VIRTUAL_ENV_PROMPT

	if [[ ! -d "$VIRTUAL_ENV" ]]; then
    log_status "No virtual environment exists. Executing \`python3 -m venv\` to create one."
		python3 -m venv "$VIRTUAL_ENV"
		"$VIRTUAL_ENV/bin/pip3" install --upgrade pip
	fi
	PATH_add "$VIRTUAL_ENV/bin"
}

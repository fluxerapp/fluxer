{ pkgs, config, lib, ... }:
{
	imports = lib.optional (builtins.pathExists ./devenv.local.nix) ./devenv.local.nix;

	env = {
		FLUXER_CONFIG = "${config.git.root}/config/config.json";
		FLUXER_DATABASE = "sqlite";
		PC_DISABLE_TUI = "1";
	};

	dotenv.enable = false;
	cachix.pull = [ "devenv" ];

	process.manager.implementation = "process-compose";

	process.managers.process-compose = {
		port = 8090;
		unixSocket.enable = true;
		settings = {
			is_tui_disabled = true;
			log_level = "info";
			log_configuration = {
				flush_each_line = true;
			};
			processes = {
				caddy = {
					command = lib.mkForce "exec ${config.git.root}/scripts/dev_process_entry.sh caddy caddy run --config ${config.git.root}/dev/Caddyfile.dev --adapter caddyfile";
					log_location = "${config.git.root}/dev/logs/caddy.log";
						availability = {
							restart = "always";
						};
				};
				css_watch = {
					command = lib.mkForce "exec ${config.git.root}/scripts/dev_process_entry.sh css_watch ${config.git.root}/scripts/dev_css_watch.sh";
					log_location = "${config.git.root}/dev/logs/css_watch.log";
						availability = {
							restart = "always";
						};
				};
					fluxer_app = {
						command = lib.mkForce "exec ${config.git.root}/scripts/dev_process_entry.sh fluxer_app env FORCE_COLOR=1 FLUXER_APP_DEV_PORT=49427 ${config.git.root}/scripts/dev_fluxer_app.sh";
						log_location = "${config.git.root}/dev/logs/fluxer_app.log";
							availability = {
								restart = "always";
							};
					};
				fluxer_gateway = {
					command = lib.mkForce "exec ${config.git.root}/scripts/dev_process_entry.sh fluxer_gateway env FLUXER_GATEWAY_NO_SHELL=1 ${config.git.root}/scripts/dev_gateway.sh";
					log_location = "${config.git.root}/dev/logs/fluxer_gateway.log";
					log_configuration = {
						flush_each_line = true;
					};
						availability = {
							restart = "always";
						};
				};
				fluxer_server = {
					command = lib.mkForce "exec ${config.git.root}/scripts/dev_process_entry.sh fluxer_server pnpm --filter fluxer_server dev";
					log_location = "${config.git.root}/dev/logs/fluxer_server.log";
						availability = {
							restart = "always";
						};
				};
				livekit = {
					command = lib.mkForce "exec ${config.git.root}/scripts/dev_process_entry.sh livekit livekit-server --config ${config.git.root}/dev/livekit.yaml";
					log_location = "${config.git.root}/dev/logs/livekit.log";
						availability = {
							restart = "always";
						};
				};
				mailpit = {
					command = lib.mkForce "exec ${config.git.root}/scripts/dev_process_entry.sh mailpit mailpit --listen 127.0.0.1:49667 --smtp 127.0.0.1:49621 --webroot /mailpit/";
					log_location = "${config.git.root}/dev/logs/mailpit.log";
						availability = {
							restart = "always";
						};
				};
			meilisearch = {
				command = lib.mkForce "MEILI_NO_ANALYTICS=true exec ${config.git.root}/scripts/dev_process_entry.sh meilisearch meilisearch --env development --master-key \"$(cat ${config.git.root}/dev/meilisearch_master_key 2>/dev/null || true)\" --db-path ${config.git.root}/dev/data/meilisearch --http-addr 127.0.0.1:7700";
					log_location = "${config.git.root}/dev/logs/meilisearch.log";
						availability = {
							restart = "always";
						};
				};
				valkey = {
					command = lib.mkForce "exec ${config.git.root}/scripts/dev_process_entry.sh valkey valkey-server --bind 127.0.0.1 --port 6379";
					log_location = "${config.git.root}/dev/logs/valkey.log";
						availability = {
							restart = "always";
						};
				};
				marketing_dev = {
					command = lib.mkForce "exec ${config.git.root}/scripts/dev_process_entry.sh marketing_dev env FORCE_COLOR=1 pnpm --filter fluxer_marketing dev";
					log_location = "${config.git.root}/dev/logs/marketing_dev.log";
						availability = {
							restart = "always";
						};
				};
				nats_core = {
					command = lib.mkForce "exec ${config.git.root}/scripts/dev_process_entry.sh nats_core nats-server -p 4222 -a 127.0.0.1";
					log_location = "${config.git.root}/dev/logs/nats_core.log";
						availability = {
							restart = "always";
						};
				};
				nats_jetstream = {
					command = lib.mkForce "exec ${config.git.root}/scripts/dev_process_entry.sh nats_jetstream nats-server -p 4223 -js -sd ${config.git.root}/dev/data/nats_jetstream -a 127.0.0.1";
					log_location = "${config.git.root}/dev/logs/nats_jetstream.log";
						availability = {
							restart = "always";
						};
				};
			};
		};
	};

	packages = with pkgs; [
		nodejs_24
		pnpm
		erlang_28
		rebar3
		valkey
		meilisearch
		nats-server
		ffmpeg
		exiftool
		caddy
		livekit
		mailpit
		go_1_24
		(rust-bin.stable."1.93.0".default.override {
			targets = [ "wasm32-unknown-unknown" ];
		})
		jq
		gettext
		lsof
		iproute2
		python3
		pkg-config
		gcc
		gnumake
		sqlite
		openssl
		curl
		uv
	];

	tasks."fluxer:bootstrap" = {
		exec = "${config.git.root}/scripts/dev_bootstrap.sh";
		before = [
			"devenv:processes:meilisearch"
			"devenv:processes:fluxer_server"
			"devenv:processes:fluxer_app"
			"devenv:processes:marketing_dev"
			"devenv:processes:css_watch"
			"devenv:processes:fluxer_gateway"
			"devenv:processes:livekit"
			"devenv:processes:mailpit"
			"devenv:processes:valkey"
			"devenv:processes:caddy"
			"devenv:processes:nats_core"
			"devenv:processes:nats_jetstream"
		];
	};

	tasks."cassandra:mig:create" = {
		exec = ''
			name="$(echo "$DEVENV_TASK_INPUT" | jq -r '.name // empty')"
			if [ -z "$name" ]; then
				echo "Missing --input name" >&2
				exit 1
			fi
			cd "${config.git.root}/fluxer_api"
			pnpm tsx scripts/CassandraMigrate.tsx create "$name"
		'';
	};

	tasks."cassandra:mig:check" = {
		exec = ''
			cd "${config.git.root}/fluxer_api"
			pnpm tsx scripts/CassandraMigrate.tsx check
		'';
	};

	tasks."cassandra:mig:status" = {
		exec = ''
			host="$(echo "$DEVENV_TASK_INPUT" | jq -r '.host // "localhost"')"
			user="$(echo "$DEVENV_TASK_INPUT" | jq -r '.user // "cassandra"')"
			pass="$(echo "$DEVENV_TASK_INPUT" | jq -r '.pass // "cassandra"')"
			cd "${config.git.root}/fluxer_api"
			pnpm tsx scripts/CassandraMigrate.tsx --host "$host" --username "$user" --password "$pass" status
		'';
	};

	tasks."cassandra:mig:up" = {
		exec = ''
			host="$(echo "$DEVENV_TASK_INPUT" | jq -r '.host // "localhost"')"
			user="$(echo "$DEVENV_TASK_INPUT" | jq -r '.user // "cassandra"')"
			pass="$(echo "$DEVENV_TASK_INPUT" | jq -r '.pass // "cassandra"')"
			cd "${config.git.root}/fluxer_api"
			pnpm tsx scripts/CassandraMigrate.tsx --host "$host" --username "$user" --password "$pass" up
		'';
	};

	tasks."licence:check" = {
		exec = ''
			cd "${config.git.root}/fluxer_api"
			pnpm tsx scripts/LicenseEnforcer.tsx
		'';
	};

	tasks."ci:py:sync" = {
		exec = ''
			cd "${config.git.root}/scripts/ci"
			uv sync --dev
		'';
	};

	tasks."ci:py:test" = {
		exec = ''
			cd "${config.git.root}/scripts/ci"
			uv run pytest
		'';
	};

	processes = {
		fluxer_server.exec = "cd ${config.git.root} && pnpm --filter fluxer_server dev";
		fluxer_app.exec = "cd ${config.git.root} && FORCE_COLOR=1 FLUXER_APP_DEV_PORT=49427 pnpm --filter fluxer_app dev";
		marketing_dev.exec = "cd ${config.git.root} && FORCE_COLOR=1 pnpm --filter fluxer_marketing dev";
		css_watch.exec = "cd ${config.git.root} && ${config.git.root}/scripts/dev_css_watch.sh";
		fluxer_gateway.exec = "cd ${config.git.root} && ${config.git.root}/scripts/dev_gateway.sh";
		meilisearch.exec = ''
			MEILI_NO_ANALYTICS=true exec meilisearch \
				--env development \
				--master-key "$(cat ${config.git.root}/dev/meilisearch_master_key 2>/dev/null || true)" \
				--db-path ${config.git.root}/dev/data/meilisearch \
				--http-addr 127.0.0.1:7700
		'';
		livekit.exec = ''
			exec livekit-server --config ${config.git.root}/dev/livekit.yaml
		'';
		mailpit.exec = ''
			exec mailpit --listen 127.0.0.1:49667 --smtp 127.0.0.1:49621 --webroot /mailpit/
		'';
		valkey.exec = "exec valkey-server --bind 127.0.0.1 --port 6379";
		caddy.exec = ''
			exec caddy run --config ${config.git.root}/dev/Caddyfile.dev --adapter caddyfile
		'';
		nats_core.exec = "exec nats-server -p 4222 -a 127.0.0.1";
		nats_jetstream.exec = ''
			exec nats-server -p 4223 -js -sd ${config.git.root}/dev/data/nats_jetstream -a 127.0.0.1
		'';
	};
}

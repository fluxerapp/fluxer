FROM chrislusf/seaweedfs:4.31 AS seaweedfs

FROM erlang:28.5.0.1

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=1000
ARG NODE_MAJOR=24
ARG ELP_VERSION=2026-02-27
ARG HELM_VERSION=4.2.0
ARG PNPM_VERSION=10.29.3
ARG WASM_BINDGEN_VERSION=0.2.122

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		bash \
		brotli \
		build-essential \
		ca-certificates \
		clang \
		cmake \
		curl \
		dbus-x11 \
		desktop-file-utils \
		dpkg-dev \
		ffmpeg \
		fakeroot \
		git \
		gnupg \
		imagemagick \
		jq \
		libasound2 \
		libatk-bridge2.0-0 \
		libavcodec-dev \
		libavfilter-dev \
		libavformat-dev \
		libavutil-dev \
		libdbus-1-dev \
		libdrm2 \
		libfido2-dev \
		libgbm1 \
		libgtk-3-0 \
		libnotify4 \
		libnss3 \
		libpipewire-0.3-dev \
		libpulse-dev \
		libsecret-1-0 \
		libudev-dev \
		libuv1-dev \
		libcurl4-openssl-dev \
		libswresample-dev \
		libswscale-dev \
		libvips-dev \
		libwayland-dev \
		libwebp-dev \
		libssl-dev \
		libx11-xcb1 \
		libxcb-dri3-0 \
		libxcomposite1 \
		libxdamage1 \
		libxkbcommon-dev \
		libxkbcommon0 \
		libxrandr2 \
		libxshmfence1 \
		libxss1 \
		libxtst6 \
		llvm \
		meson \
		ninja-build \
		openssh-client \
		pkg-config \
		protobuf-compiler \
		python3 \
		python3-pip \
		rsync \
		sudo \
		rpm \
		unzip \
		webp \
		xz-utils \
		xdg-utils \
		zstd \
	&& rm -rf /var/lib/apt/lists/*

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		apache2-utils \
		bat \
		btop \
		docker-cli \
		dnsutils \
		fd-find \
		gdb \
		htop \
		hyperfine \
		iproute2 \
		iputils-ping \
		lldb \
		lsof \
		ltrace \
		net-tools \
		netcat-openbsd \
		nmap \
		postgresql-client \
		procps \
		redis-tools \
		ripgrep \
		shellcheck \
		shfmt \
		socat \
		sqlite3 \
		strace \
		tcpdump \
		tmux \
		tree \
		valgrind \
		wrk \
		yq \
	&& ln -sf /usr/bin/fdfind /usr/local/bin/fd \
	&& ln -sf /usr/bin/batcat /usr/local/bin/bat \
	&& rm -rf /var/lib/apt/lists/*

RUN curl -fsSL https://deb.nodesource.com/setup_${NODE_MAJOR}.x | bash - \
	&& apt-get install -y --no-install-recommends nodejs \
	&& rm -rf /var/lib/apt/lists/* \
	&& corepack enable

RUN ARCH="$(dpkg --print-architecture)" \
	&& case "$ARCH" in \
		amd64) HELM_ARCH="amd64" ;; \
		arm64) HELM_ARCH="arm64" ;; \
		*) echo "Unsupported architecture for Helm: $ARCH" >&2; exit 1 ;; \
	esac \
	&& curl -fsSL "https://get.helm.sh/helm-v${HELM_VERSION}-linux-${HELM_ARCH}.tar.gz" -o /tmp/helm.tgz \
	&& tar -C /tmp -xzf /tmp/helm.tgz "linux-${HELM_ARCH}/helm" \
	&& mv "/tmp/linux-${HELM_ARCH}/helm" /usr/local/bin/helm \
	&& chmod +x /usr/local/bin/helm \
	&& rm -rf /tmp/helm.tgz "/tmp/linux-${HELM_ARCH}"

RUN python3 -m pip install --break-system-packages --no-cache-dir awscli cqlsh

COPY tools/fonts/requirements.txt /tmp/fluxer-fonts-requirements.txt
RUN python3 -m pip install --break-system-packages --no-cache-dir -r /tmp/fluxer-fonts-requirements.txt \
	&& rm /tmp/fluxer-fonts-requirements.txt \
	&& pyftsubset --help >/dev/null \
	&& python3 -c "import fontTools, brotli"

RUN if ! command -v rebar3 >/dev/null 2>&1; then \
		curl -fsSL https://s3.amazonaws.com/rebar3/rebar3 -o /usr/local/bin/rebar3 \
		&& chmod +x /usr/local/bin/rebar3; \
	fi

RUN ARCH="$(dpkg --print-architecture)" \
	&& case "$ARCH" in \
		amd64) ELP_ARCH="x86_64" ;; \
		arm64) ELP_ARCH="aarch64" ;; \
		*) echo "Unsupported architecture for ELP: $ARCH" >&2; exit 1 ;; \
	esac \
	&& curl -fsSL "https://github.com/WhatsApp/erlang-language-platform/releases/download/${ELP_VERSION}/elp-linux-${ELP_ARCH}-unknown-linux-gnu-otp-28.tar.gz" -o /tmp/elp.tgz \
	&& tar -C /usr/local/bin -xzf /tmp/elp.tgz elp \
	&& chmod +x /usr/local/bin/elp \
	&& rm /tmp/elp.tgz

RUN if ! getent group "${USER_GID}" >/dev/null; then groupadd --gid "${USER_GID}" "${USERNAME}"; fi \
	&& if ! id -u "${USERNAME}" >/dev/null 2>&1; then useradd --uid "${USER_UID}" --gid "${USER_GID}" -m "${USERNAME}" --shell /bin/bash; fi \
	&& echo "${USERNAME} ALL=(root) NOPASSWD:ALL" > "/etc/sudoers.d/${USERNAME}" \
	&& chmod 0440 "/etc/sudoers.d/${USERNAME}"

COPY --from=seaweedfs /usr/bin/weed /usr/local/bin/weed

USER ${USERNAME}

ENV DOCKER_HOST="unix:///var/run/docker.sock" \
	PATH="/home/${USERNAME}/.cargo/bin:${PATH}" \
	PNPM_HOME="/home/${USERNAME}/.local/share/pnpm"

ENV CC_wasm32_unknown_unknown="clang" \
	AR_wasm32_unknown_unknown="llvm-ar"

RUN curl -fsSL https://sh.rustup.rs | sh -s -- -y --profile default --component clippy,rustfmt \
	&& rustup target add wasm32-unknown-unknown \
	&& cargo install cargo-watch --locked \
	&& cargo install wasm-bindgen-cli --version "${WASM_BINDGEN_VERSION}" --locked

RUN corepack prepare "pnpm@${PNPM_VERSION}" --activate \
	&& pnpm --version

RUN sudo apt-get update \
	&& sudo apt-get install -y --no-install-recommends python3-venv \
	&& sudo rm -rf /var/lib/apt/lists/*

WORKDIR /workspaces/fluxer

FROM chrislusf/seaweedfs:4.31 AS seaweedfs

FROM erlang:28.5.0.1

ARG USERNAME=vscode
ARG USER_UID=1000
ARG USER_GID=1000
ARG NODE_MAJOR=24
ARG ELP_VERSION=2026-02-27
ARG PNPM_VERSION=10.29.3
ARG WASM_BINDGEN_VERSION=0.2.122

ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update \
	&& apt-get install -y --no-install-recommends \
		acl \
		bash \
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
		libimage-exiftool-perl \
		libnotify4 \
		libnss3 \
		libpipewire-0.3-dev \
		libpulse-dev \
		libsecret-1-0 \
		libudev-dev \
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
		python3 \
		python3-pip \
		python3-venv \
		sudo \
		rpm \
		unzip \
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
		docker-compose \
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

RUN curl --retry 5 --retry-delay 2 --retry-all-errors -fsSL https://deb.nodesource.com/setup_${NODE_MAJOR}.x | bash - \
	&& apt-get install -y --no-install-recommends nodejs \
	&& rm -rf /var/lib/apt/lists/* \
	&& corepack enable

RUN python3 -m pip install --break-system-packages --no-cache-dir awscli

COPY tools/fonts/requirements.txt /tmp/fluxer-fonts-requirements.txt
RUN python3 -m pip install --break-system-packages --no-cache-dir -r /tmp/fluxer-fonts-requirements.txt \
	&& rm /tmp/fluxer-fonts-requirements.txt \
	&& pyftsubset --help >/dev/null \
	&& python3 -c "import fontTools, brotli"

RUN ARCH="$(dpkg --print-architecture)" \
	&& case "$ARCH" in \
		amd64) ELP_ARCH="x86_64" ;; \
		arm64) ELP_ARCH="aarch64" ;; \
		*) echo "Unsupported architecture for ELP: $ARCH" >&2; exit 1 ;; \
	esac \
	&& curl --retry 5 --retry-delay 2 --retry-all-errors -fsSL "https://github.com/WhatsApp/erlang-language-platform/releases/download/${ELP_VERSION}/elp-linux-${ELP_ARCH}-unknown-linux-gnu-otp-28.tar.gz" -o /tmp/elp.tgz \
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

RUN curl --retry 5 --retry-delay 2 --retry-all-errors -fsSL https://sh.rustup.rs | sh -s -- -y --profile minimal --component clippy,rustfmt \
	&& rustup target add wasm32-unknown-unknown \
	&& cargo install cargo-watch --locked \
	&& cargo install wasm-bindgen-cli --version "${WASM_BINDGEN_VERSION}" --locked \
	&& rm -rf "/home/${USERNAME}/.cargo/registry" "/home/${USERNAME}/.cargo/git"

RUN corepack prepare "pnpm@${PNPM_VERSION}" --activate \
	&& pnpm --version

WORKDIR /workspaces/fluxer

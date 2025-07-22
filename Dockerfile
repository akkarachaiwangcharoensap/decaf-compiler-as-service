# ---- Base Image with LLVM 20 ----
FROM node:18-bookworm-slim AS base

# Install LLVM 20 and essential build tools
RUN apt-get update && apt-get install -y \
    wget \
    gnupg \
    curl \
    lsb-release \
    python3 \
    python3-pip \
    build-essential \
    clang \
    cmake \
    libedit-dev \
    zlib1g-dev \
    bison \
    flex \
    software-properties-common \
    ca-certificates \
 && wget -O - https://apt.llvm.org/llvm-snapshot.gpg.key | apt-key add - \
 && echo "deb http://apt.llvm.org/bookworm/ llvm-toolchain-bookworm-20 main" > /etc/apt/sources.list.d/llvm.list \
 && apt-get update \
 && apt-get install -y llvm-20 llvm-20-tools clang-20 lld-20 \
 && rm -rf /var/lib/apt/lists/*

# Set LLVM 20 as default
RUN update-alternatives --install /usr/bin/clang clang /usr/bin/clang-20 100 && \
    update-alternatives --install /usr/bin/llc llc /usr/bin/llc-20 100 && \
    update-alternatives --install /usr/bin/llvm-as llvm-as /usr/bin/llvm-as-20 100 && \
    update-alternatives --install /usr/bin/opt opt /usr/bin/opt-20 100

# Create work directory
WORKDIR /app
ENV LLVMCONFIG=llvm-config-20

# ---- Install Node.js Dependencies ----
COPY package.json yarn.lock* package-lock.json* pnpm-lock.yaml* ./
RUN \
  if [ -f yarn.lock ]; then yarn --frozen-lockfile; \
  elif [ -f package-lock.json ]; then npm ci; \
  elif [ -f pnpm-lock.yaml ]; then yarn global add pnpm && pnpm i --frozen-lockfile; \
  else echo "No lockfile found" && exit 1; \
  fi

# ---- Copy Code and Build Compiler ----
COPY . .

# Build native Decaf compiler binary (adjust path as needed)
RUN make -C src/compiler/answer decafcomp

# Ensure llvm-run is executable (if you're using it directly)
RUN chmod +x src/compiler/llvm-run

# ---- Run Server ----
EXPOSE 3000
CMD ["npx", "ts-node", "server.ts"]
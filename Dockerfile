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

# Environment
ENV LLVMCONFIG=llvm-config-20

# Set working directory
WORKDIR /app

# Install Node.js dependencies
COPY package.json yarn.lock* package-lock.json* pnpm-lock.yaml* ./
RUN \
  if [ -f yarn.lock ]; then yarn --frozen-lockfile; \
  elif [ -f package-lock.json ]; then npm ci; \
  elif [ -f pnpm-lock.yaml ]; then yarn global add pnpm && pnpm i --frozen-lockfile; \
  else echo "No lockfile found" && exit 1; \
  fi

# Install nodemon globally for hot reload (optional if in local deps)
RUN npm install -g nodemon

# Copy rest of the code
COPY . .

# Build Decaf compiler binary
RUN rm -rf src/compiler/answer/decafcomp && \
    make -C src/compiler/answer decafcomp

# Ensure llvm-run is executable
RUN chmod +x src/compiler/llvm-run

# Expose port
EXPOSE 3000

# Start in dev mode with hot reload
CMD ["npm", "run", "dev"]
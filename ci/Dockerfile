# Base image for Google Cloud Build build steps.

FROM fpco/stack-build:lts-14.16

ENTRYPOINT []

# Install `gcloud` and `gsutil`
ADD https://packages.cloud.google.com/apt/doc/apt-key.gpg google-apt-key.gpg
RUN \
  echo "deb http://packages.cloud.google.com/apt cloud-sdk-stretch main" \
  > /etc/apt/sources.list.d/google-cloud-sdk.list && \
  apt-key add google-apt-key.gpg && \
  apt-get update -yqq && \
  apt-get -yq --no-install-recommends install \
  build-essential zip unzip rpm \
  openssh-client expat libexpat1-dev xz-utils \
  google-cloud-sdk && \
  rm -rf /var/lib/apt/lists/*

RUN stack upgrade

# Install `docker`
ADD https://download.docker.com/linux/static/stable/x86_64/docker-18.09.2.tgz docker.tgz
RUN \
  tar --extract --file docker.tgz --strip-components 1 --directory /usr/local/bin/ && \
  rm docker.tgz

# Install go set GOPATH etc
ADD https://storage.googleapis.com/golang/go1.13.linux-amd64.tar.gz go.tar.gz
RUN \
  tar -xvf go.tar.gz && \
  mv go /usr/local/ && \
  chmod +x /usr/local/go/bin/*

ENV GOPATH /go
ENV PATH $GOPATH/bin:/usr/local/go/bin:$PATH

# Install ghr
RUN go get github.com/tcnksm/ghr

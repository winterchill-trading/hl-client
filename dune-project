(lang dune 3.17)

(name hl-client)

(generate_opam_files true)

(license MIT)

(maintainers "winterchill-trading")

(authors "winterchill-trading")

(source
 (github winterchill-trading/hl-client))

(package
 (name hl-client)
 (synopsis "Async client for the Hyperliquid HTTP and websocket API")
 (depends
  (ocaml
   (>= 5.2))
  dune
  core
  core_unix
  async
  async_ssl
  hl-protocol
  market-data
  venue-net
  secp256k1
  ppx_jane
  ppx_yojson_conv
  yojson
  async_websocket
  uri
  (core_bench :with-test)))

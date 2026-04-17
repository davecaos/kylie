import Config

config :kylie,
  base_url: "http://127.0.0.1:64210",
  query_path: "/api/v1/query/gizmo",
  recv_timeout: 5_000,
  connect_timeout: 5_000

if File.exists?(Path.join(__DIR__, "#{Mix.env()}.exs")) do
  import_config "#{Mix.env()}.exs"
end

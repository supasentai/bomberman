# my-project
Sever:
  stack ghci
  :l app/Server.hs
  runServer
Player 1:
  stack ghci
  :l app/NetworkedMain.hs
  main
Player 2:
  stack ghci
  :l app/NetworkedMain.hs
  main

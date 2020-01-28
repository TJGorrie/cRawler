keyDown <- function(key) return(key) # Return Keypress
mouseDown <- function(button, x, y) return(c(button, x, y)) # Return button press and locale. 1 = lmb, 2=rmb
getInput <- function(){
  capture.output(res <- getGraphicsEvent(prompt='', onKeybd=keyDown, onMouseDown=mouseDown))
  return(res)
}

getChunk <- function(pos, chunksize, mapdim){
  # Pulls map indicies for slice, will attempt to keep PC in the center
  # Unless on edges
  # pos = vector of length 2
  # chunksize = vector of length 1
  # mapdim = vector of length 2, 
  diff <- (chunksize/2) - .5 #Assuming chunksize is odd
  n <- mapdim[1] # 49
  p <- mapdim[2] # 100
  x <- pos[1]
  y <- pos[2]
  x1 <- max(1, x-diff):min(n, x+diff)
  y1 <- max(1, y-diff):min(p, y+diff)
  if(length(x1) < chunksize){
    if((x-diff) < 1 ) x1 <- 1:chunksize
    else if((x+diff) > n) x1 <- (n-chunksize+1):n
  }
  if(length(y1) < chunksize){
    if((y-diff) < 1) y1 <- 1:chunksize
    else if((y+diff) > p) y1 <- (p-chunksize+1):p
  }
  message(x1)
  message(y1)
  return(list(c1=x1, c2 = y1))
}

validMove <- function(pos, newpos, map){
  tiles <- c(4, # water
             5, # Building
             10 # Impassible
             )
  valid <- !map[newpos[1], newpos[2]] %in% tiles
  if(valid) return(newpos) else return(pos)
}

  # Map Chunks are 21 x 21 squares, or odd x odd square 
  # Edge Chunks are 1:21 x 1:21 or nrow(map)-10:nrow(map) or ncol(map)-10:ncol(map)
  # Center onto median pos?
  # 1 = you
  # 2 = path
  # 3 = grass
  # 4 = water
  # 5 = building
  # 6 = door
  # 7 = cut
  # 8 = strength
  # 9 = rock smash
  # 10 = wall/rock
state <- 0
# State: 0 = Overworld Map, 1 = In Building?
debug <- 1
pos <- c(10, 10)
chunksize <- 21
map <- as.matrix(read.csv('~/Documents/Projects/cRawler/DATA/test1.csv', header=F, row.names=NULL))
mapdim <- dim(map)
map <- map[(length(map):1)]
dim(map) <- mapdim
map2 <- map <- t(map[, (mapdim[2]:1)])
mapdim <- dim(map2)
#if(any(mapdim) < 21){
# Expand map in corners:
#for(c(1:2)[
#}
cols <- c('black', 'yellow', 'green', 'blue', 'grey', 'orange','white','white', 'white', 'pink')
map2[pos[1], pos[2]] <- 1
chunksize <- 21
chunk <- getChunk(pos, chunksize=chunksize, mapdim=mapdim)
image((map2[chunk[[1]],chunk[[2]]]), col=cols)
refreshrate <- 50
hz <- 0
while(state == 0){
  action <- getInput()
  if(length(action) == 3){ 
     message(action)
  } else {
    switch(action,
      'Right' = pos <- validMove(pos, c(min(max(1, pos[1] + 1), nrow(map)), pos[2]), map = map),
      'Left'  = pos <- validMove(pos, c(min(max(1, pos[1] - 1), nrow(map)), pos[2]), map = map),
      'Down'  = pos <- validMove(pos, c(pos[1], min(max(1, pos[2] - 1), ncol(map))), map = map),
      'Up'    = pos <- validMove(pos, c(pos[1], min(max(1, pos[2] + 1), ncol(map))), map = map),
      'm'     = { # Handle for exploring?
                image(map, col=cols[-1])
                abline(v=(pos[1]/mapdim[1]), h=(pos[2]/mapdim[2]))
                next
                }
    )
    map2 <- map
    map2[pos[1], pos[2]] <- 1
    chunk <- getChunk(pos, chunksize=chunksize, mapdim=mapdim)
    if(debug) message('X coord: ', pos[1], ' Y coord: ', pos[2],'.') 
    if(hz > refreshrate){
      hz <- 0
      image((map2[chunk[[1]],chunk[[2]]]), col=cols)
    } else {
      image((map2[chunk[[1]],chunk[[2]]]), col=cols, add=T)
      hz <- hz+1
    }
  }
}


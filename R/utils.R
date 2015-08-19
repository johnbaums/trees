fertilise <- function(size, n) {
  size <- as.integer(size)
  n <- as.integer(n)
  if(size > 25 || size < 3L) stop("Size out of valid range")
  
  # Generate integer pool and weights
  
  size0 <- size - 1L
  pool.raw <- seq.int(2L ^ size) - 1L
  pool.raw.len <- valid.unique <- length(pool.raw)
  
  # weights are a function of how many trailing zeroes each number has, for
  # example `1000` has three trailing zeroes and represnts `1000`, `100`,
  # `10`, and `1`, so it should be weighed 4x
  
  weights <- rep(1L, pool.raw.len)
  for(i in seq.int(size0))
    weights[seq.int(from=1L, to=pool.raw.len, by=2 ^ i)] <- i + 1L
  
  # Create indices to map from the "weighted" vectors to the original
  # vectors
  
  pool.vals <- rep(pool.raw, weights)
  pool.len <- length(pool.vals)
  
  # For each repeated value, what count of trailing zeros does it correspond
  # to (equivalent to: `unlist(lapply(weights, seq.int))`, but faster)
  
  z <- integer(pool.len)
  z[c(1L, cumsum(head(weights, -1L)) + 1L)] <- 1L
  w <- cumsum(!z)
  t <- cummax(z * w)
  zeros.imp <- w - t + 1L
  pad.imp <- weights[pool.vals + 1L] - zeros.imp
  
  # Generate our encoded vectors by right padding with enough zeros and then
  # adding as a value the number of zeros to the padded area
  
  zero.pad <- as.integer(2L ^ ceiling(log2(size)))
  vals.enc.init <- pool.vals * zero.pad + zeros.imp - 1L
  
  # Results tracking
  
  res <- matrix(0L, nrow=n, ncol=2L)
  res[, 2L] <- size      # leads to "" if not changed
  max.allowed <- size0   # how padded a number to pick can be
  free <- free.init <- rep(TRUE, pool.len)
  
  # Pre compute frequently used sequences and number patterns
  
  zero.mx <- as.integer(2 ^ (size - seq(size))) *
    !lower.tri(matrix(ncol=size, nrow=size))
  seqs <- lapply(1L:size, seq.int)
  seqs0 <- lapply(seqs, `-`, 1L)
  seq.rev <- rev(seq.int(size))
  seq.rev0 <- seq.rev - 1L
  ones <- rep(1L, size)
  weights.cs <- cumsum(weights)
  pool.lu <- c(1L, head(weights.cs, -1L) + 1L)
  
  # Setup our pool to draw blindly (i.e. without checking disqualification);
  
  blind.i <- 1L
  blind.len <- min(n * 3L, pool.len)        # should most likely get enough values in this draw
  blind.pool <- sample(vals.enc.init, blind.len)
  
  # Loop through the `n` requested samples
  
  for(i in seq.int(n)) {
    # Check for completeness, and remove values that would lead to incomplete
    # pools.  We only remove padded values so `valid.unique` is unchanged
    
    if(max.allowed) {
      rem.pow <- which(n - i >= valid.unique - 2L ^ seqs[[max.allowed]])
      for(j in rev.default(rem.pow)) {
        to.rem <- which(pad.imp == max.allowed)
        free[to.rem] <- FALSE
        max.allowed <- max.allowed - 1L
      }
      if(!max.allowed && n - i >= valid.unique)
        stop(
          "Logic Error: pool is not large enough to support complete samples"
        ) }
    # Pick from our shuffled pool; after we pick, if turns out value is
    # disqualified, pick again until we find a non-picked value.  Infer from
    # encoding where that value is in our original sorted list
    
    repeat {
      if(blind.i > blind.len) {  # Initial sample set not enough, add more
        which.free <- which(free)
        if(!length(which.free)) stop("Error: ran out of pool to sample")
        blind.len <- min(length(which.free), blind.len)
        blind.pool <- sample(vals.enc.init[which.free], blind.len)
        blind.i <- 1L
      }
      val.enc <- blind.pool[[blind.i]]
      val <- val.enc %/% zero.pad
      enc <- val.enc %% zero.pad
      blind.idx <- pool.lu[[val + 1L]] + enc
      if(free[[blind.idx]]) break
      blind.i <- blind.i + 1L
    }
    # Figure out how many trailing zeros our number has (recall, this is
    # encoded in the least significant bits of our number); note, zeros is a bit
    # misleading, it means: "how many digits after initial digit are explicilty
    # specied".  The name `zeros` comes from numbers like `1` that would need to
    # add zeros to be specified (e.g. `1000`, which has three zeros)
    
    weight <- weights[[val + 1L]]
    zeros <- size - weight + enc
    pad <- size0 - zeros
    res[i, ] <- c(val, pad)
    
    # Based on number of zeros, we can figure out up to what value we need
    # to disqualify (NOTE: different than withbin, here we get the next value
    # greater than our range because `free` is always same size)
    
    disq.hi.enc <- as.integer((val + 2L ^ pad)) * zero.pad
    
    # Incremental disqualification of smaller patterns by computing the
    # decimal value from a sequantially truncated bit matrix
    
    disq.loc.extra <- if(zeros) {
      seq.z <- seqs[[zeros]]
      disqual.more.tmp <- as.integer(
        ones[seq.z] %*% (
          as.integer(intToBits(val)[seq.rev])[seq.z] *
            zero.mx[seq.z, seq.z, drop=F]
        ) )
      ws <- weights[disqual.more.tmp + 1L]
      offset <- seqs0[[zeros]] + ws - size
      disq.loc <- pool.lu[disqual.more.tmp + 1L] + offset
      disqualifiable <- which(disqual.more.tmp < val)
      valid.unique <- valid.unique - sum(!(ws - offset - 1L)[disqualifiable])
      unique.default(disq.loc[disqualifiable])
    } else integer()
    
    # Find values to remove, first with the range of values disqualified by our
    # pick
    
    lo <- val.enc %/% zero.pad + 1L
    hi <- disq.hi.enc %/% zero.pad + 1L
    
    free[
      seq.int(
        from=pool.lu[[lo]],
        to=max(pool.lu[[lo]], pool.lu[[hi - 1L]] + weights[[hi - 1L]] - 1L)
      ) ] <- FALSE
    
    # Now remove any parent values
    
    free[disq.loc.extra] <- FALSE
    valid.unique <- valid.unique - 2 ^ pad
  }
  # Now convert to binary representation; note we assume ints are 32 bits
  
  res.raw <- matrix(as.integer(intToBits(res[, 1L])), nrow=32L)[seq.rev, ]
  substr(do.call(paste0, split(res.raw, row(res.raw))), 0L, size - res[, 2L])
}


# Calculate x and y coordinates in a cartesian plane, given distance and angle 
# from a given origin.
get.xy <- function(a, d, x0, y0) {
  a <- ifelse(a <= 90, 90 - a, 450 - a)
  data.frame(x = x0 + d * cos(a / 180 * pi), 
             y = y0 + d * sin(a / 180 * pi))
}
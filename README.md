# node-semver-hs
This is an almost `node-semver` compliant parser.

Although this package has not been published on Hackage yet, if you're interested in using it, please let me know.

# Differences between semver and node-semver
Semver itself defines how to order versions, but it does not specify **how to indicate the range of versions**. One of the most common implementations for version ranging is node-semver.

For instance, `1.2.3-alpha` is a valid semver, while `~1.2.3`, `>=1.2.3`, or `^1.2.3` are not. The latter represent version ranges and are not semver.

It's worth noting that some other implementations might behave differently from node-semver, even if the notation is the same.

## Usage

```haskell
import Data.SemVer.Node (parseRange, minSatisfying, maxSatisfying)
import Data.Either (fromRight)
import Debug.Trace(traceShow)
import Data.SemVer(version)
import Data.SemVer.Constraint(satisfies)

range = fromRight undefined (parseRange "^1.2.3") -- ^1.2.3 is >=1.2.3 <2.0.0-0
v_123 = version 1 2 3 [] [] 
print (satisfies v_123 range) -- True

v_124 = version 1 2 4 [] []
print $ minSatisfying range [v_123, v_124] -- Just 1.2.3
print $ maxSatisfying range [v_123, v_124] -- Just 1.2.4
```

# Known differences from node-semver

1. Ranges like "~>1.2.3" is not supported.
2. Loose parsing is not supported.
3. `IncludePreRelease` is not supported.
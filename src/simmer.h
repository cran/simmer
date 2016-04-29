#ifndef SIMMER_H
#define SIMMER_H

// [[Rcpp::depends(BH)]]
#include <Rcpp.h>

#include <boost/container/set.hpp>
#include <boost/unordered_set.hpp>
#include <boost/unordered_map.hpp>

#define VEC std::vector
#define MSET boost::container::multiset
#define USET boost::unordered_set
#define UMAP boost::unordered_map

#include <boost/foreach.hpp>

#define foreach_    BOOST_FOREACH
#define foreach_r_  BOOST_REVERSE_FOREACH

#include <boost/variant.hpp>
#include <boost/function.hpp>
#include <boost/bind.hpp>

#define FMT_0 std::setw(10) << std::right
#define FMT_11 std::setw(12) << std::right
#define FMT_12 std::setw(15) << std::left
#define FMT_21 std::setw(12) << std::right
#define FMT_22 std::setw(15) << std::left

#define PRIORITY_RELEASE    -3
#define PRIORITY_MANAGER    -2
#define PRIORITY_GENERATOR  -1

#endif

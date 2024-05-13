# dogoodr (development version)

* Use https instead of http for Do Gooder API calls.

# dogoodr 0.2.1

* `dg_actions()` and `dg_api()` gain a `page_size` argument to specify the number of records retrieved per page from the Do Gooder API. Previously 100 records were retrieved per page, this now defaults to 1000.

# dogoodr 0.2.0

* `dg_unnest()` previously performed some redundant additional cleaning on the un-nested names. This appeared to duplicate the `tidyr::unnest()` renaming rules so it has been removed. There may be some edge cases that were missed, so watch out!

* Code for processing paginated requests has been rewritten for clearer messaging and better performance.

* All messages, errors and warnings now use the cli package.

# dogoodr 0.1.0

* Initial release.

* Added a `NEWS.md` file to track changes to the package.

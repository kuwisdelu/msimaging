
# function for retrieving all data for a list of PXD ids

getPXD <- function(ids, destdir = getwd(), img = TRUE, other = TRUE) {
	require(rpx)
	for ( id in ids ) {
		message("\n!!!!------ ", id, " ------!!!!\n")
		tryCatch({
			px <- PXDataset(id)
			path <- file.path(destdir, id)
			# download imzML files or skip id if none found
			files <- pxfiles(px)
			mzml <- grep(".imzML", files, value=TRUE)
			ibd <- grep(".ibd", files, value=TRUE)
			if ( length(mzml) == 0L || length(ibd) == 0L ) {
				message("skipping ", id, " due to no imzML files\n")
				next # skip id if no imzML
			} else if ( !dir.exists(path) ) {
				dir.create(path)
			}
			pxget(px, list=mzml, destdir=path)
			pxget(px, list=ibd, destdir=path)
			# function for downloading other files
			getext <- function(...) {
				for ( pattern in list(...) ) {
					f <- grep(pattern, files, value=TRUE)
					if ( length(f) != 0L )
						pxget(px, list=f, destdir=path)	
				}
			}
			# download optical images
			if ( img )
				getext(".tif", ".png", ".jpg", ".jpeg", ".bmp")
			# download other files (but not everything)
			if ( other )
				getext(".xls", ".ppt", ".doc", ".txt", ".csv", ".mgf", ".dat")
		}, error=function(e) message("skipping ", id, "due to error\n"))
	}
}

getRefs <- function(dir) {
	require(rpx)
	ids <- list.files(dir)
	ids <- grep("PXD", ids, value=TRUE)
	ans <- lapply(ids, function(id) {
		path <- file.path(dir, id)
		ibd <- grep(".ibd", list.files(path), value=TRUE)
		ibd <- file.path(dir, id, ibd)
		size <- format(matter:::num_bytes(sum(file.size(ibd))))
		px <- PXDataset(id)
		ref <- pxref(px)[1L]
		url <- pxurl(px)[1L]
		c(id, ref, url, size)
	})
	ans <- do.call(rbind, ans)
	ans <- as.data.frame(ans)
	setNames(ans, c("id", "ref", "url", "size"))
}

# ids related to mass spectrometry imaging (with available imzML)

ids <- c("PXD001283", "PXD005197", "PXD003644", "PXD005543", "PXD005604", # 2016-12-24
	"PXD005609", "PXD005960", "PXD006108", "PXD007829", "PXD007538", # 2018-02-27
	"PXD010444", "PXD003172", "PXD011219", "PXD011104", "PXD002586", # 2018-10-24
	"PXD003339", "PXD009808", "PXD012910", "PXD010990", "PXD012379", # 2019-05-06
	"PXD013397", "PXD013860", "PXD011262", "PXD013069", "PXD010922") # 2019-09-09

# download files

library(rpx)

destdir <- "~/Documents/Datasets/PRIDE"

getPXD(ids, destdir=destdir)

refs <- getRefs(destdir)

write.csv(refs, file=file.path(destdir, "datasets.csv"), row.names=FALSE)




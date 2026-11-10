.PHONY: mesh field-%


grids:
	@echo "***Create spatial grids***"
	@Rscript R/data/grids.R
	@echo "***Finished grids.R***"

stations:
	@echo "***Create csv files for statio data***"
	@Rscript R/data/stations.R
	@echo "***Finished stations.R***"

field-%:
	@echo "***Fit latent field for variable $****"
	@Rscript R/analysis/fields.R $*
	@echo "***Finihes field estimation for $****"
	
mesh:
	@echo "***Create meshes***"
	@Rscript R/analysis/mesh.R
	@echo "***Finished mesh creation***"

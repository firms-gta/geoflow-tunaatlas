# patch-geoflow-zenodo.R
#
# Patches for the old geoflow Zenodo deposit action:
#
# 1. Fix the arguments passed to zen4R::addRelatedIdentifier()
#
# 2. Explicitly set Zenodo upload_type to "dataset" after creating
#    an empty record. Newer zen4R versions do not initialize this
#    field automatically, while the old geoflow code expects it
#    to exist.

if (requireNamespace("geoflow", quietly = TRUE)) {
  
  geoflow_path <- system.file(
    "actions/zen4R_deposit_record.R",
    package = "geoflow"
  )
  
  if (!nzchar(geoflow_path) || !file.exists(geoflow_path)) {
    
    message(
      "[patch] WARNING: geoflow zen4R_deposit_record.R not found."
    )
    
  } else {
    
    txt <- readLines(geoflow_path, warn = FALSE)
    
    # ---------------------------------------------------------------
    # PATCH 1: fix addRelatedIdentifier()
    # ---------------------------------------------------------------
    
    old_related <- which(grepl(
      'zenodo_metadata\\$addRelatedIdentifier\\("isIdenticalTo"',
      txt
    ))
    
    already_related <- any(grepl(
      "PATCH: fix Zenodo related identifier",
      txt,
      fixed = TRUE
    ))
    
    if (length(old_related) == 1) {
      
      replacement <- c(
        "    # PATCH: fix Zenodo related identifier",
        "    zenodo_metadata$addRelatedIdentifier(",
        "      identifier = entity$identifiers[[\"id\"]],",
        "      scheme = \"other\",",
        "      relation_type = \"isidenticalto\"",
        "    )"
      )
      
      txt[old_related] <- replacement
      
      message(
        "[patch] geoflow Zenodo addRelatedIdentifier() patched successfully."
      )
      
    } else if (already_related) {
      
      message(
        "[patch] geoflow Zenodo addRelatedIdentifier() already patched."
      )
      
    } else {
      
      message(
        "[patch] WARNING: expected old Zenodo addRelatedIdentifier() call not found."
      )
    }
    
    
    # ---------------------------------------------------------------
    # PATCH 2: explicitly set upload_type = "dataset"
    # ---------------------------------------------------------------
    
    already_upload_type <- any(grepl(
      "PATCH: set Zenodo upload_type",
      txt,
      fixed = TRUE
    ))
    
    if (already_upload_type) {
      
      message(
        "[patch] Zenodo upload_type already patched."
      )
      
    } else {
      
      # Locate the creation of the empty Zenodo record.
      idx <- which(grepl(
        "^\\s*zenodo_metadata\\s*<-\\s*ZENODO\\$createEmptyRecord\\(",
        txt
      ))
      
      if (length(idx) != 1) {
        
        message(
          "[patch] WARNING: could not locate createEmptyRecord() - upload_type patch not applied."
        )
        
      } else {
        
        upload_type_patch <- c(
          "",
          "    # PATCH: set Zenodo upload_type",
          "    # Newer zen4R versions do not initialize upload_type",
          "    # on a newly created empty record.",
          "    zenodo_metadata$metadata$upload_type <- \"dataset\""
        )
        
        txt <- append(
          txt,
          upload_type_patch,
          after = idx
        )
        
        message(
          "[patch] Zenodo upload_type set to 'dataset' successfully."
        )
      }
    }
    
    writeLines(txt, geoflow_path)
  }
}
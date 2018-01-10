aws acm delete-certificate --certificate-arn `cat upload-gen-cert-arn.txt | tr -d '"'` --region ${REGION} 

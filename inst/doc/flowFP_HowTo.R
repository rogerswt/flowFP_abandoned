### R code from vignette source 'flowFP_HowTo.Rnw'

###################################################
### code chunk number 1: loadPackage
###################################################

library(flowFP)


###################################################
### code chunk number 2: ShowData
###################################################
data(fs1)
fs1


###################################################
### code chunk number 3: ShowTube
###################################################
fs1[[1]]


###################################################
### code chunk number 4: Model
###################################################
mod <-  flowFPModel(fs1, name="CD45/SS Model", parameters=c(2,5), nRecursions=7)
show(mod)


###################################################
### code chunk number 5: PlotModel
###################################################
plot(mod)


###################################################
### code chunk number 6: Fingerprint
###################################################

fp <- flowFP (fs1, mod)
show(fp)
plot (fp, type="stack")


###################################################
### code chunk number 7: Plex1
###################################################

data(fs2)
mod1 <-  flowFPModel(fs1, name="CD45/SS Model vs fs1", parameters=c("SS Log", "FL3 Log"), nRecursions=7)
mod2 <-  flowFPModel(fs2, name="CD45/SS Model vs fs2", parameters=c("SS Log", "FL3 Log"), nRecursions=7)
fp1_1 <- flowFP (fs1, mod1)
fp1_2 <- flowFP (fs1, mod2)
plex <- flowFPPlex(c(fp1_1, fp1_2))
plot (plex, type='grid', vert_scale=10)


###################################################
### code chunk number 8: Plex2
###################################################

fp <- flowFP (fs1, param=c("SS Log", "FL3 Log"), nRecursions=8)
plex <- flowFPPlex()
for (levels in 8:5) {
	nRecursions(fp) <- levels
	plex <- append (plex, fp)
}
plot (plex, type="tangle", transformation="norm")


###################################################
### code chunk number 9: QC1
###################################################

fp1 <- flowFP (fs1, parameters=c("SS Log", "FL3 Log"), name="self model: fs1", nRecursions=5)
plot (fp1, type="qc", main="Gate QC for Sample fs1")


###################################################
### code chunk number 10: QC2
###################################################

fp2 <- flowFP (fs2, parameters=c("SS Log", "FL3 Log"), name="self model: fs2", nRecursions=5)
plot (fp2, type="qc", main="Gate QC for Sample fs2")


###################################################
### code chunk number 11: Subsample
###################################################
phenoData(fs2)$Tube <- factor(1:7)   #$
for (i in 1:7) {
	exprs(fs2[[i]]) = exprs(fs2[[i]])[sample(1:nrow(fs2[[i]]),size=2500),]
}
fp2 <- flowFP (fs2, param=c(2,5), nRec=5)


###################################################
### code chunk number 12: FvizFigure
###################################################
xyplot (`FL3 Log` ~ `SS Log` | Tube, data=fs2)


###################################################
### code chunk number 13: flowFP_HowTo.Rnw:493-494
###################################################
plot(trellis.last.object())


###################################################
### code chunk number 14: QC3
###################################################
plot (fp2, fs2, hi=5, showbins=c(6,7,10,11), pch=20, cex=.3, transformation='norm')


###################################################
### code chunk number 15: QC4
###################################################
data(plate)
fp <- flowFP (plate, parameters=c("SSC-H","FL3-H","FL4-H"), nRecursions=5)
plot (fp, type='plate')



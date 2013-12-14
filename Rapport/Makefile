TARGET=rapport_BC_DG.pdf

all: $(TARGET)

showpdf: all
        evince $(TARGET)

$(TARGET): $(TARGET:%.pdf=%.tex)
        pdflatex $< -o $@
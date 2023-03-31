#!/bin/bash

############################### from the cell paper, 26 genomes 

wget https://download.cncb.ac.cn/gwh/Plants/Glycine_soja_SoyW01_asm_GWHACDY00000000/GWHACDY00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_soja_SoyW02_asm_GWHACDZ00000000/GWHACDZ00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_soja_SoyW03_asm_GWHACEA00000000/GWHACEA00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyL01_asm_GWHACEB00000000/GWHACEB00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyL02_asm_GWHACEC00000000/GWHACEC00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyL03_asm_GWHACED00000000/GWHACED00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyL04_asm_GWHACEE00000000/GWHACEE00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyL05_asm_GWHACEF00000000/GWHACEF00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyL06_asm_GWHACEG00000000/GWHACEG00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyL07_asm_GWHACEH00000000/GWHACEH00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyL08_asm_GWHACEI00000000/GWHACEI00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyL09_asm_GWHACEJ00000000/GWHACEJ00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC01_asm_GWHACEK00000000/GWHACEK00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC02_asm_GWHACEL00000000/GWHACEL00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC03_asm_GWHACEM00000000/GWHACEM00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC04_asm_GWHACEN00000000/GWHACEN00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC05_asm_GWHACEO00000000/GWHACEO00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC06_asm_GWHACEP00000000/GWHACEP00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC07_asm_GWHACEQ00000000/GWHACEQ00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC08_asm_GWHACER00000000/GWHACER00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC09_asm_GWHACES00000000/GWHACES00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC10_asm_GWHACET00000000/GWHACET00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC11_asm_GWHACEU00000000/GWHACEU00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC12_asm_GWHACEV00000000/GWHACEV00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC13_asm_GWHACEW00000000/GWHACEW00000000.genome.fasta.gz
wget https://download.cncb.ac.cn/gwh/Plants/Glycine_max_SoyC14_asm_GWHACEX00000000/GWHACEX00000000.genome.fasta.gz


for file in GWHACDY00000000	GWHACDZ00000000	GWHACEA00000000	GWHACEB00000000	GWHACEC00000000	GWHACED00000000	GWHACEE00000000	GWHACEF00000000	GWHACEG00000000	GWHACEH00000000	GWHACEI00000000	GWHACEJ00000000	GWHACEK00000000	GWHACEL00000000	GWHACEM00000000	GWHACEN00000000	GWHACEO00000000	GWHACEP00000000	GWHACEQ00000000	GWHACER00000000	GWHACES00000000	GWHACET00000000	GWHACEU00000000	GWHACEV00000000	GWHACEW00000000	GWHACEX00000000

do

gunzip -k "$file".genome.fasta.gz

perl -p -i -e 's/GWH.*OriSeqID=Chr0/chr/g' "$file".genome.fasta
perl -p -i -e 's/GWH.*OriSeqID=Chr/chr/g' "$file".genome.fasta
perl -p -i -e 's/\tLen.*//g' "$file".genome.fasta
mv "$file".genome.fasta "$file"

done

mv		GWHACDY00000000		SoyW01.fa
mv		GWHACDZ00000000		SoyW02.fa
mv		GWHACEA00000000		SoyW03.fa
mv		GWHACEB00000000		SoyL01.fa
mv		GWHACEC00000000		SoyL02.fa
mv		GWHACED00000000		SoyL03.fa
mv		GWHACEE00000000		SoyL04.fa
mv		GWHACEF00000000		SoyL05.fa
mv		GWHACEG00000000		SoyL06.fa
mv		GWHACEH00000000		SoyL07.fa
mv		GWHACEI00000000		SoyL08.fa
mv		GWHACEJ00000000		SoyL09.fa
mv		GWHACEK00000000		SoyC01.fa
mv		GWHACEL00000000		SoyC02.fa
mv		GWHACEM00000000		SoyC03.fa
mv		GWHACEN00000000		SoyC04.fa
mv		GWHACEO00000000		SoyC05.fa
mv		GWHACEP00000000		SoyC06.fa
mv		GWHACEQ00000000		SoyC07.fa
mv		GWHACER00000000		SoyC08.fa
mv		GWHACES00000000		SoyC09.fa
mv		GWHACET00000000		SoyC10.fa
mv		GWHACEU00000000		SoyC11.fa
mv		GWHACEV00000000		SoyC12.fa
mv		GWHACEW00000000		SoyC13.fa
mv		GWHACEX00000000		SoyC14.fa

#################################################### from soybase

wget https://soybase.org/data/v2/Glycine/max/genomes/Wm82.gnm4.4PTR/glyma.Wm82.gnm4.4PTR.genome_main.fna.gz
wget https://soybase.org/data/v2/Glycine/max/genomes/Lee.gnm1.BXNC/glyma.Lee.gnm1.BXNC.genome_main.fna.gz
wget https://soybase.org/data/v2/Glycine/max/genomes/Zh13.gnm1.N6C8/glyma.Zh13.gnm1.N6C8.genome_main.fna.gz
wget https://soybase.org/data/v2/Glycine/soja/genomes/PI483463.gnm1.YJWS/glyso.PI483463.gnm1.YJWS.genome_main.fna.gz
wget https://soybase.org/data/v2/Glycine/soja/genomes/W05.gnm1.SVL1/glyso.W05.gnm1.SVL1.genome_main.fna.gz


gunzip -k glyma.Wm82.gnm4.4PTR.genome_main.fna.gz
perl -p -i -e 's/glyma.Wm82.gnm4.Gm0/chr/g' glyma.Wm82.gnm4.4PTR.genome_main.fna
perl -p -i -e 's/glyma.Wm82.gnm4.Gm/chr/g' glyma.Wm82.gnm4.4PTR.genome_main.fna
mv glyma.Wm82.gnm4.4PTR.genome_main.fna Wm82.fa


gunzip -k glyma.Lee.gnm1.BXNC.genome_main.fna.gz
perl -p -i -e 's/glyma.Lee.gnm1.Gm0/chr/g' glyma.Lee.gnm1.BXNC.genome_main.fna
perl -p -i -e 's/glyma.Lee.gnm1.Gm/chr/g' glyma.Lee.gnm1.BXNC.genome_main.fna
mv glyma.Lee.gnm1.BXNC.genome_main.fna Lee.fa


gunzip -k glyma.Zh13.gnm1.N6C8.genome_main.fna.gz
perl -p -i -e 's/glyma.Zh13.gnm1.Chr0/chr/g' glyma.Zh13.gnm1.N6C8.genome_main.fna
perl -p -i -e 's/glyma.Zh13.gnm1.Chr/chr/g' glyma.Zh13.gnm1.N6C8.genome_main.fna
perl -p -i -e 's/\tGWHAA.*//g' glyma.Zh13.gnm1.N6C8.genome_main.fna
mv glyma.Zh13.gnm1.N6C8.genome_main.fna ZH13.fa


gunzip -k glyso.PI483463.gnm1.YJWS.genome_main.fna.g
perl -p -i -e 's/glyso.PI483463.gnm1.Gs0/chr/g' glyso.PI483463.gnm1.YJWS.genome_main.fna
perl -p -i -e 's/glyso.PI483463.gnm1.Gs/chr/g' glyso.PI483463.gnm1.YJWS.genome_main.fna
mv glyso.PI483463.gnm1.YJWS.genome_main.fna PI483463.fa


gunzip -k glyso.W05.gnm1.SVL1.genome_main.fna.gz
perl -p -i -e 's/glyso.W05.gnm1.Chr0/chr/g' glyso.W05.gnm1.SVL1.genome_main.fna
perl -p -i -e 's/glyso.W05.gnm1.Chr/chr/g' glyso.W05.gnm1.SVL1.genome_main.fna
perl -p -i -e 's/ QZWG.*//g' glyso.W05.gnm1.SVL1.genome_main.fna
mv glyso.W05.gnm1.SVL1.genome_main.fna W05.fa

####################################################

# prepare chr files for BRIDGEcereal

dir_Soybean='/home/xianran_li/bridgecereal/database/Soybean/'
mkdir $dir_Soybean

dir_candidate='/home/xianran_li/bridgecereal/candidate_dir/Soybean/'
mkdir $dir_candidate

dir_candidate_='/home/xianran_li/bridgecereal/candidate_dir/Soybean/Candidate_genes/'
mkdir $dir_candidate_


#sp_dir='/home/xianran_li/bridgecereal/database/Soybean/'
 sp_dir='/mnt/946c1663-fcbd-4a78-8887-c55f23c5b496/bszhang/database_soybean/'

 line_codes="Lee	PI483463	SoyC01	SoyC02	SoyC03	SoyC04	SoyC05	SoyC06	SoyC07	SoyC08	SoyC09	SoyC10	SoyC11	SoyC12	SoyC13	SoyC14	SoyL01	SoyL02	SoyL03	SoyL04	SoyL05	SoyL06	SoyL07	SoyL08	SoyL09	SoyW01	SoyW02	SoyW03	W05	Wm82	ZH13"

for line_code in $line_codes

 do
    echo "processing $line_code ..."

     line_dir=$sp_dir$line_code
     if [ ! -e $line_dir ]; then
     mkdir $line_dir
     fi

  gb_fa=$sp_dir$line_code'.fa'

 # gb_gz=$gb_fa'.gz'
 # gunzip -k $gb_gz

  samtools faidx $gb_fa

    for ch in {1..20}

    do

    ch_fa=$line_dir'/'$line_code'_chr'$ch'.fa'
    ch_gz=$ch_fa'.gz'

    if [ ! -e $ch_gz ]; then
    samtools faidx $gb_fa 'chr'$ch -o $ch_fa
    makeblastdb -in $ch_fa -dbtype nucl
    bgzip $ch_fa -@ 8
    samtools faidx $ch_gz
    fi

    done

  rm $gb_fa
  rm $gb_fa".fai"

done

#############################
gff_Soybean='/home/xianran_li/bridgecereal/gff/Soybean/'
mkdir $gff_Soybean


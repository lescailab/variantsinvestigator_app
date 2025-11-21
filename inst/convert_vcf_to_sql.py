import sqlite3
import os
import gzip
import argparse

def create_connection(db_file):
    con = sqlite3.connect(db_file)
    con.execute("PRAGMA journal_mode = OFF")
    con.execute("PRAGMA synchronous = OFF")
    return con

def prepare_database(con):
    cur = con.cursor()
    cur.execute("DROP TABLE IF EXISTS variants")
    cur.execute("DROP TABLE IF EXISTS genotypes")
    cur.execute('''
        CREATE TABLE variants (
            id TEXT, chromosome TEXT, ref TEXT, alt TEXT, filter TEXT,
            ac TEXT, allele TEXT, consequence TEXT, impact TEXT, symbol TEXT,
            gene TEXT, feature TEXT, exon TEXT, intron TEXT, strand TEXT,
            variantclass TEXT, sift TEXT, polyphen TEXT, clin_sig TEXT,
            lof TEXT, lof_filter TEXT, lof_flags TEXT, pubmed TEXT,
            gnomad_af REAL, qual REAL, start INTEGER, end INTEGER, width INTEGER
        )
    ''')
    cur.execute('''
        CREATE TABLE genotypes (
            sample TEXT, chromosome TEXT, ref TEXT, alt TEXT, gt TEXT, pl TEXT, ad TEXT,
            start INTEGER, end INTEGER, width INTEGER, dp INTEGER, gq INTEGER,
            variant_id INTEGER
        )
    ''')
    con.commit()

def parse_info(info_str):
    info_data = {}
    for data in info_str.split(";"):
        if "=" in data:
            key, value = data.split("=", 1)
            info_data[key.lower()] = value
    return info_data

def parse_genotypes(sample_names, format_keys, sample_values, chromosome, ref, alt, start, end, width, variant_id):
    genotype_records = []
    for i, sample_val in enumerate(sample_values):
        vals = sample_val.split(":")
        fmt = dict(zip(format_keys, vals))
        gt = fmt.get("GT", "")
        ad = fmt.get("AD", "")
        pl = fmt.get("PL", "")
        dp = int(fmt["DP"]) if "DP" in fmt and fmt["DP"].isdigit() else None
        gq = int(fmt["GQ"]) if "GQ" in fmt and fmt["GQ"].isdigit() else None

        genotype_records.append((
            sample_names[i], chromosome, ref, alt, gt, pl, ad,
            start, end, width, dp, gq, variant_id
        ))
    return genotype_records

def parse_vcf(vcf_file, con):
    cur = con.cursor()
    with gzip.open(vcf_file, 'rt') if vcf_file.endswith(".gz") else open(vcf_file, 'r') as f:
        sample_names = []
        cur.execute("BEGIN TRANSACTION")
        for line in f:
            line = line.strip()
            if line.startswith("##"):
                continue
            elif line.startswith("#CHROM"):
                sample_names = line.split("\t")[9:]
            else:
                fields = line.split("\t")
                chrom, pos, vid, ref, alt, qual, filt, info = fields[:8]
                format_str = fields[8] if len(fields) > 8 else ""
                sample_values = fields[9:] if len(fields) > 9 else []

                pos = int(pos)
                end = pos + len(ref) - 1
                width = end - pos + 1
                qual_val = float(qual) if qual.replace('.', '', 1).isdigit() else None

                info_data = parse_info(info)

                gnomad_af = float(info_data.get("gnomad_af", "nan")) if info_data.get("gnomad_af", "nan").replace('.', '', 1).isdigit() else None

                variant_record = (
                    vid, chrom, ref, alt, filt,
                    info_data.get("ac"), info_data.get("allele"), info_data.get("consequence"),
                    info_data.get("impact"), info_data.get("symbol"), info_data.get("gene"),
                    info_data.get("feature"), info_data.get("exon"), info_data.get("intron"),
                    info_data.get("strand"), info_data.get("variantclass"), info_data.get("sift"),
                    info_data.get("polyphen"), info_data.get("clin_sig"), info_data.get("lof"),
                    info_data.get("lof_filter"), info_data.get("lof_flags"), info_data.get("pubmed"),
                    gnomad_af, qual_val, pos, end, width
                )

                cur.execute('''
                    INSERT INTO variants (
                        id, chromosome, ref, alt, filter, ac, allele, consequence, impact,
                        symbol, gene, feature, exon, intron, strand, variantclass,
                        sift, polyphen, clin_sig, lof, lof_filter, lof_flags, pubmed,
                        gnomad_af, qual, start, end, width )
                    VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                ''', variant_record)

                variant_id = cur.lastrowid

                if format_str and sample_values:
                    format_keys = format_str.split(":")
                    genotypes = parse_genotypes(sample_names, format_keys, sample_values,
                                                chrom, ref, alt, pos, end, width, variant_id)
                    cur.executemany('''
                        INSERT INTO genotypes (
                            sample, chromosome, ref, alt, gt, pl, ad, start, end, width, dp, gq, variant_id)
                        VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)
                    ''', genotypes)
        con.commit()

def convertVCFtoSQL(vcf, database):
    if os.path.exists(database):
        os.remove(database)

    con = create_connection(database)
    prepare_database(con)
    parse_vcf(vcf, con)
    con.close()


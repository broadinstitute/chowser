# Chowser

App to munge genomic data (TSV or VCF), including filtering by p-value, canoncalizing variant ids, matching variants across files, selecting preset variant ids, identifying consolidated regions around significant variants, clip data files to given region, turn p-values to Z-scores and convert table of correlations into matrix form.

Written in Scala.

# Usage

    Usage: chowser tsv|vcf ... 
      -h, --help      Show help message
      -v, --version   Show version of this program

    Subcommand: tsv
    Usage: chowser tsv filter [OPTIONS]
    Consume tab-separated file
      -h, --help   Show help message
  
    Subcommand: tsv range
    usage: chowser tsv range [OPTIONS]
    Filter records by numeric range
      -c, --col  <arg>   Name of column to apply condition
      -g, --gt  <arg>    Retain records with value greater than given value
      -i, --in  <arg>    Input file
      -l, --lt  <arg>    Retain records with value less than given value
      -o, --out  <arg>   Output file
      -h, --help         Show help message
    Subcommand: tsv slice
    usage: chowser tsv slice [OPTIONS]
    Filter records by numeric range
      -c, --col  <arg>     Name of column to apply condition
      -i, --in  <arg>      Input file
      -o, --out  <arg>     Output file
      -v, --value  <arg>   Required value
      -h, --help           Show help message
    Subcommand: tsv sort
    usage: chowser tsv sort [OPTIONS]
    Sort records of tab-separated file
      -c, --col  <arg>   Name of column to sort by
      -i, --in  <arg>    Input file
      -o, --out  <arg>   Output file
      -h, --help         Show help message
    Subcommand: tsv extract-unique
    usage: chowser tsv extract-unique [OPTIONS]
    Extract unique values of a column of tab-separated file
      -c, --col  <arg>   Name of column
      -i, --in  <arg>    Input file
      -o, --out  <arg>   Output file
      -h, --help         Show help message
    Subcommand: variants
    Usage: chowser variants regions [OPTIONS]
    Consume file containing variants
      -h, --help   Show help message
  
    Subcommand: variants regions
      -c, --chrom-col  <arg>   Name of column containing chromosome
      -i, --in  <arg>          Input file
      -o, --out  <arg>         Output file
      -p, --pos-col  <arg>     Name of column containing position
      -r, --radius  <arg>      Minimum distance to be included on each side.
      -h, --help               Show help message
    Subcommand: variants for-region
          --chrom  <arg>       Chromosome on which region lies.
      -c, --chrom-col  <arg>   Name of column containing chromosome
      -e, --end  <arg>         End position of region.
      -i, --in  <arg>          Input file
      -o, --out  <arg>         Output file
      -p, --pos-col  <arg>     Name of column containing position
      -s, --start  <arg>       Start position of region.
      -h, --help               Show help message
    Subcommand: variants for-region-by-id
      -c, --chrom  <arg>    Chromosome on which region lies.
      -e, --end  <arg>      End position of region.
          --id-col  <arg>   Name of column containing variant ids
      -i, --in  <arg>       Input file
      -o, --out  <arg>      Output file
      -s, --start  <arg>    Start position of region.
      -h, --help            Show help message
    Subcommand: variants canonicalize-vcf
      -i, --in  <arg>    Input file
      -o, --out  <arg>   Output file
      -h, --help         Show help message
    Subcommand: variants canonicalize-tsv
      -a, --alt-col  <arg>     Name of column containing alternate allele.
      -c, --chrom-col  <arg>   Name of column containing chromosome
          --id-col  <arg>      Name of column containing variant ids
      -i, --in  <arg>          Input file
      -o, --out  <arg>         Output file
      -p, --pos-col  <arg>     Name of column containing position
      -r, --ref-col  <arg>     Name of column containing reference allele.
      -h, --help               Show help message
    Subcommand: variants match-vcf-tsv
      -i, --id-col  <arg>     Variant id column name in TSV file
          --in-both  <arg>    Output file with variants both in VCF and TSV file.
      -t, --tsv  <arg>        Input TSV file
          --tsv-only  <arg>   Output file with variants only in TSV file.
      -v, --vcf  <arg>        Input VCF file
          --vcf-only  <arg>   Output file with variants only in VCF file.
      -h, --help              Show help message  
    Subcommand: variants match-tsv-tsv
      -i, --id-col1  <arg>     Variant id column name in TSV file 1
          --id-col2  <arg>     Variant id column name in TSV file 2
          --in-both  <arg>     Output file with variants in both TSV files.
      -t, --tsv1  <arg>        Input VCF file
          --tsv1-only  <arg>   Output file with variants only in TSV file 1.
          --tsv2  <arg>        Input TSV file
          --tsv2-only  <arg>   Output file with variants only in TSV file 2.
      -h, --help               Show help message
    Subcommand: variants select-tsv
      -d, --data  <arg>               File with data to to select.
      -i, --id-col-data  <arg>        Column with variant ids in data.
          --id-col-selection  <arg>   Column with variant ids in selection.
      -o, --out  <arg>                Output file
      -s, --selection  <arg>          Ids of variants to select..
      -h, --help                      Show help message
    Subcommand: variants select-vcf
      -d, --data  <arg>               File with data to to select.
      -i, --id-col-selection  <arg>   Column with variant ids in selection.
      -o, --out  <arg>                Output file
      -s, --selection  <arg>          Ids of variants to select..
      -h, --help                      Show help message
    Subcommand: caviar
      -h, --help   Show help message

    Subcommand: caviar matrix
    usage: chowser tsv matrix [OPTIONS]
    Reshape list of values into matrix form
          --id-col1  <arg>       Name of first id column in values file.
          --id-col2  <arg>       Name of second id column in values file.
      -i, --ids-file  <arg>      VCF File for ids
      -o, --out  <arg>           Output file
          --value-col  <arg>     Name of value column in values file.
      -v, --values-file  <arg>   File with values
      -h, --help                 Show help message
    Subcommand: caviar p-to-z
          --id-col  <arg>   Name of id column in p-values file.
      -i, --in  <arg>       Input file
      -o, --out  <arg>      Output file
      -p, --p-col  <arg>    Name of p-value column in p-values file.
      -h, --help            Show help message
    Subcommand: shell
      -h, --help   Show help message

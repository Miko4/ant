
open XNum;
open Runtime;
open Job;

value output_pages job pages = match pages with
[ [] -> ()
| _  -> do
  {
    let comment = " ant output "
                ^ string_of_int (job.time.Unix.tm_year + 1900) ^ "."
                ^ string_of_int (job.time.Unix.tm_mon + 1) ^ "."
                ^ string_of_int job.time.Unix.tm_mday ^ ":"
                ^ string_of_int job.time.Unix.tm_hour
                ^ string_of_int job.time.Unix.tm_min;

    match job.output_format with
    [ DVI  -> GenerateDVI.write_dvi_file               job.output_file comment pages
    | XDVI -> GenerateDVI.write_xdvi_file              job.output_file comment pages
    | PDF  -> GeneratePDF.write_pdf_file               job.output_file comment pages
    | PS   -> GeneratePostScript.write_postscript_file job.output_file comment pages
    | SVG  -> GenerateSVG.write_svg_file               job.output_file comment pages
    ]
  }
];


$pdf_previewer = 'start zathura';
$print_type = 'pdf';
$pdf_mode = 1;
$pdflatex = 'pdflatex -interaction=nonstopmode -file-line-error -synctex=1 %O %S';
add_cus_dep('idx', 'ind', 0, 'makeindex');
sub makeindex{
  system("makeindex \"$_[0].idx\"");
}
#!perl -wl
#expect the output of pgmhist
$_=<>;
die unless /^value\s+count\s+b/;
$_=<>;
die unless /^-+\s+-+\s+/;
while(<>){
    chomp;
    @F=split;
    die unless $F[0] =~ /^\d+$/;
    $first=$F[0] unless defined$first;
    $last=$F[0];
}
unless($first==$last or ($first==0 and $last==255)){
    print "-bvalue $first -wvalue $last";
}

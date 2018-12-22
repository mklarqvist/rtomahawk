#include <string>
#include <sstream>

#include <Rcpp.h>
using namespace Rcpp;

#include "tomahawk.h"
#include "utility.h"
#include "two_reader.h"

List rcpp_hello_world() {
    CharacterVector x = CharacterVector::create( "foo", "bar" )  ;
    NumericVector y   = NumericVector::create( 0.0, 1.0 ) ;
    List z            = List::create( x, y ) ;

    return z ;
}

// [[Rcpp::export]]
std::string twk_version(){
    return(tomahawk::TOMAHAWK_LIB_VERSION);
}

// [[Rcpp::export]]
Rcpp::DataFrame twk_head(Rcpp::S4& obj, uint32_t n_records){
    if (! obj.inherits("twk"))
        stop("Input must be a twk() model object.");

    if(n_records == 0){
         Rcpp::Rcout << tomahawk::utility::timestamp("ERROR") << "Cannot head 0 records!" << std::endl;
        return Rcpp::DataFrame::create();
    }

    // New instance of reader.
    tomahawk::two_reader oreader;

    // Open file handle.
    if(oreader.Open(Rcpp::as<std::string>(obj.slot("file.path"))) == false){
        Rcpp::Rcout << tomahawk::utility::timestamp("ERROR") << "Failed to open: \"" << Rcpp::as<std::string>(obj.slot("file.path")) << "\"!" << std::endl;
        return Rcpp::DataFrame::create();
    }

    // Peek at index
    uint32_t n_avail = 0;
    for(int i = 0; i < oreader.index.n; ++i){
        n_avail += oreader.index.ent[i].n;
        if(n_avail >= n_records) break;
    }
    n_records = std::min(n_avail, n_records);

    std::vector<uint32_t>    cols[3];
    std::vector<std::string> scols[2];
    std::vector<double>      dcols[11];
    for(int i = 0; i < 3;  ++i) cols[i].resize(n_records);
    for(int i = 0; i < 2;  ++i) scols[i].resize(n_records);
    for(int i = 0; i < 11; ++i) dcols[i].resize(n_records);

    int i = 0;
    while(oreader.NextRecord()){
        // Integer
        cols[0][i]  = oreader.it.rcd->controller;
        cols[1][i]  = oreader.it.rcd->Apos;
        cols[2][i]  = oreader.it.rcd->Bpos;
        // String
        scols[0][i] = oreader.hdr.contigs_[oreader.it.rcd->ridA].name;
        scols[1][i] = oreader.hdr.contigs_[oreader.it.rcd->ridB].name;
        // Double
        dcols[0][i]  = oreader.it.rcd->cnt[0];
        dcols[1][i]  = oreader.it.rcd->cnt[1];
        dcols[2][i]  = oreader.it.rcd->cnt[2];
        dcols[3][i]  = oreader.it.rcd->cnt[3];
        dcols[4][i]  = oreader.it.rcd->D;
        dcols[5][i]  = oreader.it.rcd->Dprime;
        dcols[6][i]  = oreader.it.rcd->R;
        dcols[7][i]  = oreader.it.rcd->R2;
        dcols[8][i]  = oreader.it.rcd->P;
        dcols[9][i]  = oreader.it.rcd->ChiSqFisher;
        dcols[10][i] = oreader.it.rcd->ChiSqModel;

        if(++i == n_records) break;
    }

    return(Rcpp::DataFrame::create(Rcpp::Named("FLAG")=cols[0],
                                    Rcpp::Named("ridA")=scols[0],
                                    Rcpp::Named("posA")=cols[1],
                                    Rcpp::Named("ridB")=scols[1],
                                    Rcpp::Named("posB")=cols[2],
                                    Rcpp::Named("REFREF")=dcols[0],
                                    Rcpp::Named("REFALT")=dcols[1],
                                    Rcpp::Named("ALTREF")=dcols[2],
                                    Rcpp::Named("ALTALT")=dcols[3],
                                    Rcpp::Named("D")=dcols[4],
                                    Rcpp::Named("Dprime")=dcols[5],
                                    Rcpp::Named("R")=dcols[6],
                                    Rcpp::Named("R2")=dcols[7],
                                    Rcpp::Named("P")=dcols[8],
                                    Rcpp::Named("ChiSqFisher")=dcols[9],
                                    Rcpp::Named("ChiSqModel")=dcols[10]));
}

// [[Rcpp::export]]
Rcpp::DataFrame twk_tail(Rcpp::S4& obj){
    if (! obj.inherits("twk"))
        stop("Input must be a twk() model object.");

    // New instance of reader.
    tomahawk::two_reader oreader;

    // Open file handle.
    if(oreader.Open(Rcpp::as<std::string>(obj.slot("file.path"))) == false){
        Rcpp::Rcout << tomahawk::utility::timestamp("ERROR") << "Failed to open: \"" << Rcpp::as<std::string>(obj.slot("file.path")) << "\"!" << std::endl;
        return Rcpp::DataFrame::create();
    }

    oreader.stream->seekg(oreader.index.ent[oreader.index.n - 1].foff);
    if(oreader.stream->good() == false){
        Rcpp::Rcout << tomahawk::utility::timestamp("ERROR") << "Failed to seek in file: \"" << Rcpp::as<std::string>(obj.slot("file.path")) << "\"!" << std::endl;
        return Rcpp::DataFrame::create();
    }
    
    std::vector<uint32_t>    cols[3];
    std::vector<std::string> scols[2];
    std::vector<double>      dcols[11];
    for(int i = 0; i < 3;  ++i) cols[i].resize(oreader.index.ent[oreader.index.n - 1].n);
    for(int i = 0; i < 2;  ++i) scols[i].resize(oreader.index.ent[oreader.index.n - 1].n);
    for(int i = 0; i < 11; ++i) dcols[i].resize(oreader.index.ent[oreader.index.n - 1].n);

    int i = 0;
    while(oreader.NextRecord()){
        // Integer
        cols[0][i]  = oreader.it.rcd->controller;
        cols[1][i]  = oreader.it.rcd->Apos;
        cols[2][i]  = oreader.it.rcd->Bpos;
        // String
        scols[0][i] = oreader.hdr.contigs_[oreader.it.rcd->ridA].name;
        scols[1][i] = oreader.hdr.contigs_[oreader.it.rcd->ridB].name;
        // Double
        dcols[0][i]  = oreader.it.rcd->cnt[0];
        dcols[1][i]  = oreader.it.rcd->cnt[1];
        dcols[2][i]  = oreader.it.rcd->cnt[2];
        dcols[3][i]  = oreader.it.rcd->cnt[3];
        dcols[4][i]  = oreader.it.rcd->D;
        dcols[5][i]  = oreader.it.rcd->Dprime;
        dcols[6][i]  = oreader.it.rcd->R;
        dcols[7][i]  = oreader.it.rcd->R2;
        dcols[8][i]  = oreader.it.rcd->P;
        dcols[9][i]  = oreader.it.rcd->ChiSqFisher;
        dcols[10][i] = oreader.it.rcd->ChiSqModel;
        ++i;
    }

    return(Rcpp::DataFrame::create(Rcpp::Named("FLAG")=cols[0],
                                    Rcpp::Named("ridA")=scols[0],
                                    Rcpp::Named("posA")=cols[1],
                                    Rcpp::Named("ridB")=scols[1],
                                    Rcpp::Named("posB")=cols[2],
                                    Rcpp::Named("REFREF")=dcols[0],
                                    Rcpp::Named("REFALT")=dcols[1],
                                    Rcpp::Named("ALTREF")=dcols[2],
                                    Rcpp::Named("ALTALT")=dcols[3],
                                    Rcpp::Named("D")=dcols[4],
                                    Rcpp::Named("Dprime")=dcols[5],
                                    Rcpp::Named("R")=dcols[6],
                                    Rcpp::Named("R2")=dcols[7],
                                    Rcpp::Named("P")=dcols[8],
                                    Rcpp::Named("ChiSqFisher")=dcols[9],
                                    Rcpp::Named("ChiSqModel")=dcols[10]));
}

bool LoadContigs(const tomahawk::two_reader& oreader, Rcpp::S4& obj){
    // Start constructing contig component.
    std::vector<uint32_t>    idx;
    std::vector<std::string> name, desc;
    std::vector<int64_t>     bases;

    for(uint32_t i = 0; i < oreader.hdr.contigs_.size(); ++i){
        idx.push_back(oreader.hdr.contigs_[i].idx);
        name.push_back(oreader.hdr.contigs_[i].name);
        desc.push_back(oreader.hdr.contigs_[i].description.size() == 0 ? "NA" : oreader.hdr.contigs_[i].description);
        bases.push_back(oreader.hdr.contigs_[i].n_bases);
    }

    obj.slot("contigs") = Rcpp::DataFrame::create(Rcpp::Named("idx")=idx,
                                                  Rcpp::Named("name")=name,
                                                  Rcpp::Named("desc")=desc,
                                                  Rcpp::Named("bases")=bases);

    return(true);
}

bool LoadSamples(const tomahawk::two_reader& oreader, Rcpp::S4& obj){
     // Start constructing the sample component.
    std::vector<std::string> name;

    for(int i = 0; i < oreader.hdr.samples_.size(); ++i){
        name.push_back(oreader.hdr.samples_[i]);
    }

    obj.slot("samples") = name;
    return(true);
}

bool LoadIndex(const tomahawk::two_reader& oreader, Rcpp::S4& obj){
    obj.slot("state") = (int)oreader.index.state;

    std::vector<int32_t>  rid;
    std::vector<uint32_t> n, minpos, maxpos, b_unc, b_cmp;
    std::vector<uint64_t> foff, fend;

    for(int i = 0; i < oreader.index.n; ++i){
        rid.push_back(oreader.index.ent[i].rid);
        n.push_back(oreader.index.ent[i].n);
        minpos.push_back(oreader.index.ent[i].minpos);
        maxpos.push_back(oreader.index.ent[i].maxpos);
        b_unc.push_back(oreader.index.ent[i].b_unc);
        b_cmp.push_back(oreader.index.ent[i].b_cmp);
        foff.push_back(oreader.index.ent[i].foff);
        fend.push_back(oreader.index.ent[i].fend);
    }

    obj.slot("records") = Rcpp::DataFrame::create(Rcpp::Named("rid")=rid,
                                                  Rcpp::Named("n")=n,
                                                  Rcpp::Named("minpos")=minpos,
                                                  Rcpp::Named("maxpos")=maxpos,
                                                  Rcpp::Named("b_unc")=b_unc,
                                                  Rcpp::Named("b_cmp")=b_cmp,
                                                  Rcpp::Named("foff")=foff,
                                                  Rcpp::Named("fend")=fend);

    return(true);
}

bool LoadHeaderLiterals(const tomahawk::two_reader& oreader, Rcpp::S4& obj){
    // Parse literals
    std::istringstream st(oreader.hdr.literals_);
    std::string line;    
    std::vector<std::string> literals;
    while (std::getline(st, line)) {
        if(line.size()) literals.push_back(line);
    }

    obj.slot("literals") = literals;
    return(true);
}

/**
 * @brief Retrieves information from the Tomahawk output file header.
 * 
 * @param input String input file
 * @return Rcpp::DataFrame 
 */
// [[Rcpp::export]]
Rcpp::S4 LoadHeader(std::string input){
    // Make use of R internal function path.expand() to expand out
    // a relative path into an absolute path as required by the API.
    Function f("path.expand");
    std::string inreal = Rcpp::as<std::string>(f(input));

    // Test
    Rcpp::Language twk_type("new", "twk");
    Rcpp::S4 twk( twk_type.eval() ); //use Rcpp::Language to create and assign a twk_header S4 object.

    // If there is no input data
    if(input.size() == 0)
        return(twk);

    Rcpp::Language hdr("new", "twk_header");
    Rcpp::S4 header( hdr.eval() );
    Rcpp::Language idx("new", "twk_index");
    Rcpp::S4 index( idx.eval() );

    // New instance of reader.
    tomahawk::two_reader oreader;

    // Open file handle.
    if(oreader.Open(inreal) == false){
        Rcpp::Rcout << tomahawk::utility::timestamp("ERROR") << "Failed to open: \"" << inreal << "\"!" << std::endl;
        return(twk);
    //return Rcpp::DataFrame::create();
    }

    // Start constructing contig component.
    LoadContigs(oreader, header);
    LoadSamples(oreader, header);
    LoadIndex(oreader, index);
    LoadHeaderLiterals(oreader, header);

    twk.slot("file.path") = inreal;
    twk.slot("index") = index;
    twk.slot("header") = header;

    // Start constructing the sample component.
    return(twk);
}
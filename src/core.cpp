#include <string>
#include <sstream>
#include <regex>

#include <Rcpp.h>

#include "tomahawk.h"
#include "utility.h"
#include "two_reader.h"
#include "ld.h"

// [[Rcpp::export(name=".checkIntervalContig")]]
bool CheckIntervalContig(const std::string& interval){
    return(std::regex_match(interval, tomahawk::TWK_REGEX_CONTIG_ONLY));
}

// [[Rcpp::export(name=".checkIntervalContigPosition")]]
bool CheckIntervalContigPosition(const std::string& interval){
    return(std::regex_match(interval, tomahawk::TWK_REGEX_CONTIG_POSITION));
}

// [[Rcpp::export(name=".checkIntervalContigRange")]]
bool CheckIntervalContigRange(const std::string& interval){
    return(std::regex_match(interval, tomahawk::TWK_REGEX_CONTIG_RANGE));
}

// [[Rcpp::export(name=".checkInterval")]]
int CheckInterval(const std::string& interval){
    if(CheckIntervalContig(interval)) return(1);
    else if(CheckIntervalContigPosition(interval)) return(2);
    else if(CheckIntervalContigRange(interval)) return(3);
    return(-1);
}

/**
 * @brief Supportive structure for transposing internal tomahawk::twk1_two_t 
 *        records stored in a record-centric fashion into field-centric (column) 
 *        stores as required by R data.frame structures.
 */
struct twk_two_transpose_t {
    twk_two_transpose_t(uint32_t n) : include_flag(~(uint16_t)0)
    {
        // Todo: fix reserves when not all FLAGs are set.
        for(int i = 0; i < 3;  ++i) cols[i].reserve(n);
        for(int i = 0; i < 2;  ++i) scols[i].reserve(n);
        for(int i = 0; i < 11; ++i) dcols[i].reserve(n);
    }
  
    /**
     * @brief Overload operator for adding individual twk1_two_t records to
     *        each individual column store.
     * 
     * @param rec     Reference input twk1_two_t record.
     * @param oreader Reference twk_reader used for extracting contig names from the header.
     */
    void Add(const tomahawk::twk1_two_t& rec, const tomahawk::two_reader& oreader){
        // Integer
        cols[0].push_back(rec.controller);
        cols[1].push_back(rec.Apos + 1); // 1-base output.
        cols[2].push_back(rec.Bpos + 1); // 1-base output.
        // String
        scols[0].push_back(oreader.hdr.contigs_[rec.ridA].name);
        scols[1].push_back(oreader.hdr.contigs_[rec.ridB].name);
        // Double
        dcols[0].push_back(rec.cnt[0]);
        dcols[1].push_back(rec.cnt[1]);
        dcols[2].push_back(rec.cnt[2]);
        dcols[3].push_back(rec.cnt[3]);
        dcols[4].push_back(rec.D);
        dcols[5].push_back(rec.Dprime);
        dcols[6].push_back(rec.R);
        dcols[7].push_back(rec.R2);
        dcols[8].push_back(rec.P);
        dcols[9].push_back(rec.ChiSqFisher);
        dcols[10].push_back(rec.ChiSqModel);
    }

    /**
     * @brief Constructs a Rcpp::DataFrame from the internal column vectors
     *        representing twk1_two_t records
     * 
     * @return * Rcpp::DataFrame 
     */
    Rcpp::DataFrame GetDataFrame() const {
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

public:
    uint16_t include_flag;
    std::vector<uint32_t>    cols[3];
    std::vector<std::string> scols[2];
    std::vector<double>      dcols[11];
};

// [[Rcpp::export(name=".twk_version")]]
std::string twk_version(){
    return(tomahawk::LibrariesString());
}

// [[Rcpp::export(name=".twk_head")]]
Rcpp::DataFrame twk_head(Rcpp::S4& obj, uint32_t n_records){
    if (! obj.inherits("twk"))
        Rcpp::stop("Input must be a twk() model object.");

    if(n_records == 0)
        Rcpp::stop("Cannot head 0 records!");

    // New instance of reader.
    tomahawk::two_reader oreader;

    // Open file handle.
    if(oreader.Open(Rcpp::as<std::string>(obj.slot("file.path"))) == false){
        Rcpp::stop(tomahawk::utility::timestamp("ERROR") + "Failed to open: \"" + Rcpp::as<std::string>(obj.slot("file.path")) + "\"!");
    }

    // Peek at index
    uint32_t n_avail = 0;
    for(int i = 0; i < oreader.index.n; ++i){
        n_avail += oreader.index.ent[i].n;
        if(n_avail >= n_records) break;
    }
    n_records = std::min(n_avail, n_records);

    twk_two_transpose_t recs(n_records);

    int i = 0;
    while(oreader.NextRecord()){
        recs.Add(*oreader.it.rcd, oreader);
        if(++i == n_records) break;
    }

    return(recs.GetDataFrame());
}

// [[Rcpp::export(name=".twk_tail")]]
Rcpp::DataFrame twk_tail(Rcpp::S4& obj, uint32_t n_records){
    if (! obj.inherits("twk"))
        Rcpp::stop("Input must be a twk() model object.");

    // New instance of reader.
    tomahawk::two_reader oreader;

    // Open file handle.
    if(oreader.Open(Rcpp::as<std::string>(obj.slot("file.path"))) == false){
        Rcpp::stop(tomahawk::utility::timestamp("ERROR") + "Failed to open: \"" + Rcpp::as<std::string>(obj.slot("file.path")) + "\"!");
    }

    uint32_t n_avail = 0;
    uint32_t n_block = (oreader.index.n - 1), n_offset = 0, n_blocks = 1;
    for(int i = (oreader.index.n - 1); i != 0; --i){
        n_avail += oreader.index.ent[i].n;
        if(n_avail >= n_records) break;
        --n_block; ++n_blocks;
    }
    if(n_records > n_avail) n_records = n_avail;

    oreader.stream->seekg(oreader.index.ent[n_block].foff);
    oreader.it.NextBlock();
    if(n_blocks == 1 && oreader.index.ent[n_block].n >= n_records){
        oreader.it.offset = oreader.index.ent[n_block].n - n_records;
    } else {
        int32_t diff = n_avail - n_records;
        oreader.it.offset = diff;
    }

    if(oreader.stream->good() == false){
        Rcpp::Rcout << tomahawk::utility::timestamp("ERROR") << "Failed to seek in file: \"" << Rcpp::as<std::string>(obj.slot("file.path")) << "\"!" << std::endl;
        Rcpp::stop("failed");
    }
    
    twk_two_transpose_t recs(n_records);

    int i = 0;
    while(oreader.NextRecord()){
        recs.Add(*oreader.it.rcd, oreader);
        if(++i == n_records) break;
    }

    return(recs.GetDataFrame());
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
 * @param input     String input file.
 * @return Rcpp::S4 Returns an R S4-class instance of `twk`.
 */
// [[Rcpp::export(name=".OpenTomahawkOutput")]]
Rcpp::S4 OpenTomahawkOutput(std::string input){
    // Make use of R internal function path.expand() to expand out
    // a relative path into an absolute path as required by the API.
    Rcpp::Function f("path.expand");
    std::string inreal = Rcpp::as<std::string>(f(input));

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
        Rcpp::stop(tomahawk::utility::timestamp("ERROR") + "Failed to open: \"" + inreal + "\"!");
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

// [[Rcpp::export(name=".twk_decay")]]
Rcpp::DataFrame twk_decay(Rcpp::S4& obj, uint32_t range, uint32_t n_bins){
    if (! obj.inherits("twk"))
        Rcpp::stop("Input must be a twk() model object.");

    // New instance of reader.
    tomahawk::two_reader oreader;

    // Open file handle.
    if(oreader.Open(Rcpp::as<std::string>(obj.slot("file.path"))) == false){
        Rcpp::stop(tomahawk::utility::timestamp("ERROR") + "Failed to open: \"" + Rcpp::as<std::string>(obj.slot("file.path")) + "\"!");
    }

	uint32_t n_range_bin = range/n_bins;
	std::vector<std::pair<double,uint64_t>> decay(n_bins+1,{0,0});

	while(oreader.NextRecord()){
		// Same contig only.
		if(oreader.it.rcd->ridA == oreader.it.rcd->ridB){
			// Upper trig only.
			if(oreader.it.rcd->Apos < oreader.it.rcd->Bpos){
				decay[std::min((oreader.it.rcd->Bpos - oreader.it.rcd->Apos) / n_range_bin, n_bins)].first += oreader.it.rcd->R2;
				++decay[std::min((oreader.it.rcd->Bpos - oreader.it.rcd->Apos) / n_range_bin, n_bins)].second;
			}
		}
	}

    std::vector<uint32_t> from, to;
    std::vector<double> mean_out;
    std::vector<uint64_t> freq;

	for(int i = 0; i < decay.size(); ++i){
        from.push_back((i*n_range_bin)); 
        to.push_back(((i+1)*n_range_bin));
        mean_out.push_back(decay[i].first/std::max(decay[i].second,(uint64_t)1));
        freq.push_back(decay[i].second);
	}

    return(Rcpp::DataFrame::create(Rcpp::Named("from")=from,
                                Rcpp::Named("to")=to,
                                Rcpp::Named("mean")=mean_out,
                                Rcpp::Named("freq")=freq));
}

Rcpp::S4 twk_agg_to_s4(const tomahawk::twk1_aggregate_t& agg){
    // Create return class.
    Rcpp::Language twk_type("new", "twk_agg");
    Rcpp::S4 twk( twk_type.eval() ); //use Rcpp::Language to create and assign a twk_header S4 object.

    // Transmute the C++ twk1_aggregate_t struct into the S4 `twk_agg` class.
    twk.slot("n") = agg.n;
    twk.slot("x") = agg.x;
    twk.slot("y") = agg.y;
    twk.slot("bpx") = agg.bpx;
    twk.slot("bpy") = agg.bpy;
    twk.slot("n_original") = agg.n_original;
    twk.slot("range") = agg.range;

    // Construct R-version of data matrix.
    Rcpp::NumericMatrix data(agg.x, agg.y);
    for(uint32_t i = 0; i < agg.n; ++i) data[i] = agg.data[i];
    twk.slot("data") = data;

    // Construct offset data.frame from a vector of structs.
    Rcpp::NumericVector rid_range(agg.rid_offsets.size()), rid_min(agg.rid_offsets.size()), rid_max(agg.rid_offsets.size());
    for(uint32_t i = 0; i < agg.rid_offsets.size(); ++i){
        rid_range[i] = agg.rid_offsets[i].range;
        rid_min[i]   = agg.rid_offsets[i].min;
        rid_max[i]   = agg.rid_offsets[i].max;
    }
    twk.slot("offsets") = Rcpp::DataFrame::create(
                            Rcpp::Named("range")=rid_range,
                            Rcpp::Named("min")=rid_min,
                            Rcpp::Named("max")=rid_max);

    return(twk);
}

/**
 * @brief Reads a binary twk1_aggregate_t file from disk and converts it into
 *        rtomahawk S4 version `twk_agg`.
 * 
 * @param input     Input string to file path containing the target aggregate file.
 * @return Rcpp::S4 Returns a `twk_agg` S4-class.
 */
// [[Rcpp::export(name=".twk_read_aggregate")]]
Rcpp::S4 twk_read_aggregate(const std::string input){
    if(input.size() == 0)
        Rcpp::stop("Input path cannot be empty!");

    // Make use of R internal function path.expand() to expand out
    // a relative path into an absolute path as required by the API.
    Rcpp::Function f("path.expand");
    std::string inreal = Rcpp::as<std::string>(f(input));

    tomahawk::twk1_aggregate_t agg;
    if(agg.Open(inreal) == false)
        Rcpp::stop("Failed to open file");

    return(twk_agg_to_s4(agg));
}

// [[Rcpp::export(name=".twk_aggregate")]]
Rcpp::S4 twk_aggregate(const Rcpp::S4& twk,
                       std::string agg_name, std::string red_name,
                       int32_t xbins, int32_t ybins, int32_t min_count,
                       int32_t threads = 1,
                       bool verbose = false, bool progress = false)
{
    if (! twk.inherits("twk"))
        Rcpp::stop("Input must be a twk() model object.");

    if(Rcpp::as<std::string>(twk.slot("file.path")).size() == 0)
        Rcpp::stop("Input path cannot be empty!");

    if(agg_name.size() == 0){
		Rcpp::stop("No aggregation function provided...");
	}

	if(red_name.size() == 0){
		Rcpp::stop("No reduce function provided...");
	}

	if(min_count < 0){
		Rcpp::stop("Cannot have a min-cutoff < 0...");
	}

	if(threads <= 0){
		Rcpp::stop("Cannot have <= 0 threads..");
	}

    if(xbins <= 0){
		Rcpp::stop("Cannot have <= 0 xbins..");
	}

    if(ybins <= 0){
		Rcpp::stop("Cannot have <= 0 ybins..");
	}

    tomahawk::twk_two_settings settings;
    settings.in = Rcpp::as<std::string>(twk.slot("file.path"));
    settings.n_threads = threads;

    tomahawk::two_reader oreader;
    tomahawk::twk1_aggregate_t agg;
    if(oreader.Aggregate(agg, settings, agg_name, red_name, xbins, ybins, min_count, verbose, progress) == false)
        Rcpp::stop("failed");

    Rcpp::S4 ret = twk_agg_to_s4(agg);
    ret.slot("twk") = twk;
    ret.slot("aggregation") = agg_name;
    ret.slot("reduction") = red_name;

    return(ret);
}

/**
 * @brief Calculate the pairwise linkage-disequilibrium coefficients for a 
 *        target region vs its neighbourhood. Note that enabling the progress
 *        flag will detach a progress thread that will NOT terminate if run in
 *        R!
 * 
 * @param twk       Input Tomahawk file.
 * @param interval  Target interval string formatted as "CHR:POS".
 * @param window    Neighbourhood in base-pairs.
 * @param minP      Largest P-value to report.
 * @param minR2     Smallest R2-value to report.
 * @param threads   Number of threads used during unpacking and compute.
 * @param verbose   Should messages be written to std::cerr?
 * @param progress  Should progress be reported?
 * @return Rcpp::S4 Returns a R S4-class of type `twk_agg`.
 */
// [[Rcpp::export(name=".twk_scalc")]]
Rcpp::S4 twk_scalc(const Rcpp::S4& twk, 
                   std::string interval, 
                   int32_t window, 
                   double minP = 1, 
                   double minR2 = 0.05,
                   int32_t threads = 1, 
                   bool verbose = false, 
                   bool progress = false)
{
    if (! twk.inherits("twk"))
        Rcpp::stop("Input must be a twk() model object.");

    if(Rcpp::as<std::string>(twk.slot("file.path")).size() == 0)
        Rcpp::stop("Input path cannot be empty!");

    // Make use of R internal function tempfile() to expand out
    // a tempfile required to store the output at.
    Rcpp::Function f("tempfile");
    std::string tempfile = Rcpp::as<std::string>(f(""));

    if(tempfile.size() == 0){
		Rcpp::stop("Must provide an output path...");
	}
    
    if(window <= 1){
        Rcpp::stop("Cannot have window <= 1...");
    }

	if(threads <= 0){
		Rcpp::stop("Cannot have <= 0 threads...");
	}

    if(minP < 0 || minP > 1){
		Rcpp::stop("Cannot have 0 < P > 1 P value threshold...");
	}

    if(minR2 < 0 || minR2 > 1){
		Rcpp::stop("Cannot have 0 < P > 1 R2 value threshold...");
	}

    tomahawk::twk_ld_settings settings;
    settings.in = Rcpp::as<std::string>(twk.slot("file.path"));
    settings.out = tempfile + ".two";
    settings.n_threads = threads;
    settings.ival_strings.push_back(interval);
    settings.l_surrounding = window;
    settings.minP = minP;
    settings.minR2 = minR2;
    settings.single = true;

    tomahawk::twk_ld ld;
	if(ld.ComputeSingle(settings, verbose, progress) == false)
        Rcpp::stop("failed");
	
    // Iff success then load new 'twk' object.
    Rcpp::Language twk_type("new", "twk");
    Rcpp::S4 otwk( twk_type.eval() ); //use Rcpp::Language to create and assign a twk_header S4 object.
    Rcpp::Language hdr("new", "twk_header");
    Rcpp::S4 header( hdr.eval() );
    Rcpp::Language idx("new", "twk_index");
    Rcpp::S4 index( idx.eval() );
    Rcpp::Language datac("new", "twk_data");
    Rcpp::S4 data( datac.eval() );

    // New instance of reader.
    tomahawk::two_reader oreader;

    // Open file handle.
    if(oreader.Open(settings.out) == false){
        Rcpp::stop(tomahawk::utility::timestamp("ERROR") + "Failed to open: \"" + settings.out + "\"!");
    }

    // Start constructing contig component.
    LoadContigs(oreader, header);
    LoadSamples(oreader, header);
    LoadIndex(oreader, index);
    LoadHeaderLiterals(oreader, header);

    otwk.slot("file.path") = settings.out;
    otwk.slot("index") = index;
    otwk.slot("header") = header;

    // Peek at index
    uint32_t n_avail = 0;
    for(int i = 0; i < oreader.index.n; ++i)
        n_avail += oreader.index.ent[i].n;

    twk_two_transpose_t recs(n_avail);

    int i = 0;
    while(oreader.NextRecord()){
        recs.Add(*oreader.it.rcd, oreader);
    }
    data.slot("data") = recs.GetDataFrame();
    otwk.slot("data") = data;

    // Delete temporary file after loading into memory.
    Rcpp::Function unlink("unlink");
    unlink(settings.out);

    return(otwk);
}

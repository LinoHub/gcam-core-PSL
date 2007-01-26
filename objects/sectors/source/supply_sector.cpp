/*! 
* \file supply_sector.cpp
* \ingroup Objects
* \brief SupplySector class source file.
* \author James Blackwood
*/

#include "util/base/include/definitions.h"
#include <string>
#include <cassert>

// xml headers
#include <xercesc/dom/DOMNode.hpp>

#include "util/base/include/xml_helper.h"
#include "sectors/include/supply_sector.h"
#include "containers/include/scenario.h"
#include "marketplace/include/marketplace.h"
#include "sectors/include/subsector.h"
#include "util/logger/include/ilogger.h"
#include "marketplace/include/imarket_type.h"
#include "util/base/include/configuration.h"
#include "containers/include/iinfo.h"
#include "sectors/include/sector_utils.h"
#include "util/base/include/summary.h"
#include "reporting/include/indirect_emissions_calculator.h"

using namespace std;
using namespace xercesc;

extern Scenario* scenario;

// static initialize.
const string SupplySector::XML_NAME = "supplysector";

/* \brief Constructor
* \param aRegionName The name of the region.
*/
SupplySector::SupplySector( const string& aRegionName )
: Sector ( aRegionName ),
mHasTrialSupply( false ),
mBiomassAdder( scenario->getModeltime()->getmaxper() )
{
}

/*! \brief Initialize the SupplySector.
* \details Currently only calls the base class initCalc.
* \param aNationalAccount National accounts container.
* \param aDemographics Regional demographics object.
* \param aPeriod Period for which to initialize the SupplySector.
*/
void SupplySector::initCalc( NationalAccount* aNationalAccount,
                            const Demographic* aDemographics,
                            const int aPeriod )
{
    Sector::initCalc( aNationalAccount, aDemographics, aPeriod );

    // Check if the sector should create a trial supply market. First check if
    // the flag is already set. This is only done in period 1 so that other
    // markets have a chance to set the flag.
    if( aPeriod == 1 ){
        if( !mHasTrialSupply ){
            const Marketplace* marketplace = scenario->getMarketplace();

            // Always look for the flag in period 0.
            const IInfo* sectorInfo = marketplace->getMarketInfo( name, regionName, 0, true );

            // The sector's market info must exist.
            assert( sectorInfo );

            // Set the trial supply to true if the marketinfo has a request. 
            mHasTrialSupply = sectorInfo->getBoolean( "create-trial-supply", false );
        }

        // Create the trial supply market if requested.
        if( mHasTrialSupply ){
            SectorUtils::createTrialSupplyMarket( regionName, name );
        }
    }
}

/*! \brief returns Sector output.
*
* Returns the total amount of the SupplySector. 
*
* \author Sonny Kim
* \param period Model period
* \todo make year 1975 regular model year so that logic below can be removed
* \return total output
*/
double SupplySector::getOutput( const int aPeriod ) const {
    double output = 0;
    for ( unsigned int i = 0; i < subsec.size(); ++i ) {
        double subsecOutput = subsec[ i ]->getOutput( aPeriod );
        // error check.
        if ( !util::isValidNumber( subsecOutput ) ){
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::ERROR );
            mainLog << "Output for subsector " << subsec[ i ]->getName() << " in Sector " << name 
                << " in region " << regionName <<" is not valid." << endl;
            continue;
        }
        output += subsecOutput;
    }

    // In the base period return a read in output if there is none.
    if( aPeriod == 0 && output == 0 ){
        return mBaseOutput;
    }

    return output;
}

/*! \brief Return the price of the SupplySector.
* \details The price of a SupplySector is the weighted average subsector price.
* \param aPeriod Model period.
* \return Price.
* \todo Move entire calculation here once demand sectors are rewritten.
*/
double SupplySector::getPrice( const GDP* aGDP, const int aPeriod ) const {
    return Sector::getPrice( aGDP, aPeriod );
}

/*! \brief Calculate the final supply price.
* \details Calculates shares for the sector and price for the supply sector, and
*          then sets the price of the good into the marketplace.
* \param aGDP The regional GDP container.
* \param aPeriod The period in which to calculate the final supply price.
*/
void SupplySector::calcFinalSupplyPrice( const GDP* aGDP, const int aPeriod ){
    // Calculate the costs for all subsectors.
    calcCosts( aPeriod );

    // Set the price into the market.
    Marketplace* marketplace = scenario->getMarketplace();

    double avgMarginalPrice = getPrice( aGDP, aPeriod );

    // Temporary hack for CCTP.
    if( name == "regional biomass" ){
        // Adjust for biomass carbon value adder.
        // Prices must be adjusted by the price multiplier if there is a
        // carbon tax in place.
        double carbonPrice = marketplace->getPrice( "CO2", regionName, aPeriod, false );
        if( carbonPrice != Marketplace::NO_MARKET_PRICE && carbonPrice > 0 ){
            // The carbon price and all crops except biomass are in 1990
            // dollars. Biomass must be adjusted by a 1975 carbon price.
            const double CONVERT_90_TO_75 = 2.212;
            
            avgMarginalPrice += mBiomassAdder[ aPeriod ] * carbonPrice / CONVERT_90_TO_75;
        }
    }
    
    marketplace->setPrice( name, regionName, avgMarginalPrice, aPeriod, true );
}

/*! \brief Set supply Sector output
* \details This routine takes the market demand and propagates that through the
*          supply sub-sectors where it is shared out (and subsequently passed to
*          the technology level within each sub-Sector to be shared out).
* \author Sonny Kim
* \param aGDP GDP object uses to calculate various types of GDPs.
* \param aPeriod Model period
*/
void SupplySector::supply( const GDP* aGDP, const int aPeriod ) {
    Marketplace* marketplace = scenario->getMarketplace();
    // demand for the good produced by this Sector
    double marketDemand = marketplace->getDemand( name, regionName, aPeriod );

    // Determine if fixed output must be scaled because fixed supply
    // exceeded demand.
    double fixedOutput = getFixedOutput( aPeriod );
    double scaleFactor = SectorUtils::calcFixedOutputScaleFactor( marketDemand, fixedOutput );

    // Calculate the demand for new investment.
    double newInvestment = max( marketDemand - fixedOutput, 0.0 );
    const vector<double> subsecShares = calcSubsectorShares( aGDP, aPeriod );

    // This is where subsector and technology outputs are set
    for( unsigned int i = 0; i < subsec.size(); ++i ){
        // set subsector output from Sector demand
        subsec[ i ]->setOutput( subsecShares[ i ] * newInvestment, scaleFactor, aGDP, aPeriod );
    }    

    const static bool debugChecking = Configuration::getInstance()->getBool( "debugChecking" );
    if ( debugChecking ) {
        // If the model is working correctly this should never give an error
        // An error here means that the supply summed up from the supply sectors 
        // is not equal to the demand that was passed in 
        double mrksupply = getOutput( aPeriod );

        // if demand identically = 1 then must be in initial iteration so is not an error
        if ( aPeriod > 0 && fabs(mrksupply - marketDemand ) > 0.01 && marketDemand != 1 ) {
            ILogger& mainLog = ILogger::getLogger( "main_log" );
            mainLog.setLevel( ILogger::WARNING );
            mainLog << regionName << " Market "<<  name << " demand and derived supply are not equal by: ";
            mainLog << fabs( mrksupply - marketDemand ) << ": ";
            mainLog << "S: " << mrksupply << " D: " << marketDemand << " Fixed-Supply: " << getFixedOutput( aPeriod ) << endl;
        }
    }

    // Set the trial supply if the market exists.
    if( mHasTrialSupply ){
        SectorUtils::setTrialSupply( regionName, name, getOutput( aPeriod ), aPeriod );
    }
}

/*! \brief Complete the initialization of the supply sector.
* \param aRegionInfo Regional information object.
* \param aDependencyFinder Regional dependency finder.
* \param aGlobalTechDB Global technology database.
*/
void SupplySector::completeInit( const IInfo* aRegionInfo,
                                 DependencyFinder* aDependencyFinder,
                                 ILandAllocator* aLandAllocator,
                                 const GlobalTechnologyDatabase* aGlobalTechDB )
{
    // default unit to EJ
    if ( mOutputUnit.empty() ) {
        mOutputUnit = "EJ"; 
    }
    // default unit to EJ
    if ( mInputUnit.empty() ) {
        mInputUnit = "EJ"; 
    }
    // default unit to $/GJ
    if ( mPriceUnit.empty() ) {
        mPriceUnit = "1975$/GJ"; 
    }
    Sector::completeInit( aRegionInfo, aDependencyFinder, aLandAllocator, aGlobalTechDB );
    setMarket();
}

/*! \brief Get the XML node name for output to XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* This function may be virtual to be overridden by derived class pointers.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME.
*/
const string& SupplySector::getXMLName() const {
    return XML_NAME;
}

/*! \brief Get the XML node name in static form for comparison when parsing XML.
*
* This public function accesses the private constant string, XML_NAME.
* This way the tag is always consistent for both read-in and output and can be easily changed.
* The "==" operator that is used when parsing, required this second function to return static.
* \note A function cannot be static and virtual.
* \author Josh Lurz, James Blackwood
* \return The constant XML_NAME as a static.
*/
const std::string& SupplySector::getXMLNameStatic() {
    return XML_NAME;
}

/*! \brief XML output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML.
* This function is called by toInputXML in the base Sector class.
*
* \author Steve Smith, Josh Lurz, Sonny Kim
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void SupplySector::toInputXMLDerived( ostream& out, Tabs* tabs ) const {  

    // Temporary CCTP hack.
    XMLWriteVector( mBiomassAdder, "biomass-price-adder", out, tabs, scenario->getModeltime(), 0.0 );
}

/*! \brief XML debugging output stream for derived classes
*
* Function writes output due to any variables specific to derived classes to XML.
* This function is called by toInputXML in the base Sector class.
*
* \author Steve Smith, Josh Lurz, Sonny Kim
* \param out reference to the output stream
* \param tabs A tabs object responsible for printing the correct number of tabs. 
*/
void SupplySector::toDebugXMLDerived( const int period, ostream& out, Tabs* tabs ) const {

    // write the xml for the class members.
    XMLWriteElement( mBiomassAdder[ period ], "biomass-price-adder", out, tabs );
}
/*! \brief Parses any child nodes specific to derived classes
*
* Method parses any input data from child nodes that are specific to the classes derived from this class. Since Sector is the generic base class, there are no values here.
*
* \author Josh Lurz, Steve Smith
* \param nodeName name of current node
* \param curr pointer to the current node in the XML input tree
*/
bool SupplySector::XMLDerivedClassParse( const string& nodeName, const DOMNode* curr ) {
    // Temporary hack for CCTP.
    if( nodeName == "biomass-price-adder" ){
        XMLHelper<double>::insertValueIntoVector( curr, mBiomassAdder, scenario->getModeltime() );
    }
    else {
        return false;
    }
    return true;
}

/*! \brief Create new market for this Sector
*
* Sets up the appropriate market within the marketplace for this Sector. Note that the type of market is NORMAL -- 
* signifying that this market is a normal market that is solved (if necessary).
*
* \author Sonny Kim, Josh Lurz, Steve Smith
*/
void SupplySector::setMarket() {    
    Marketplace* marketplace = scenario->getMarketplace();
    // Creates a regional market. MiniCAM supply sectors are not independent and 
    // cannot be members of multi-region markets.
    if( marketplace->createMarket( regionName, regionName, name, IMarketType::NORMAL ) ) {
        marketplace->setPrice( name, regionName, mBasePrice, 0, true );

        // Set price and output units for period 0 market info
        IInfo* marketInfo = marketplace->getMarketInfo( name, regionName, 0, true );
        marketInfo->setString( "price-unit", mPriceUnit );
        marketInfo->setString( "output-unit", mOutputUnit );
    }
}

//! Write MiniCAM style Sector output to database.
void SupplySector::dbOutput( const GDP* aGDP,
                             const IndirectEmissionsCalculator* aIndEmissCalc ) const
{
    const Modeltime* modeltime = scenario->getModeltime();
    // function protocol
    void dboutput4(string var1name,string var2name,string var3name,string var4name,
        string uname,vector<double> dout);

    // total Sector output
    int maxper = modeltime->getmaxper();
    vector<double> temp(maxper);
    for( int per = 0; per < maxper; ++per ){
        temp[ per ] = getOutput( per );
    }
    dboutput4( regionName,"Secondary Energy Prod","by Sector",name,mOutputUnit, temp );
    dboutput4( regionName,"Secondary Energy Prod",name,"zTotal",mOutputUnit, temp );


    string str; // temporary string

    // Sector fuel consumption by fuel type
    typedef map<string,double>:: const_iterator CI;
    map<string,double> tfuelmap = summary[0].getfuelcons();
    for (CI fmap=tfuelmap.begin(); fmap!=tfuelmap.end(); ++fmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_fmap_second(fmap->first);
        }
        if( fmap->first == "" ){
            dboutput4( regionName,"Fuel Consumption",name, "No Fuelname", mInputUnit,temp);
        }
        else {
            dboutput4( regionName,"Fuel Consumption",name,fmap->first,mInputUnit,temp);
        }
    }

    // Sector emissions for all greenhouse gases
    map<string,double> temissmap = summary[0].getemission(); // get gases for per 0
    for (CI gmap=temissmap.begin(); gmap!=temissmap.end(); ++gmap) {
        for (int m=0;m<maxper;m++) {
            temp[m] = summary[m].get_emissmap_second(gmap->first);
        }
        dboutput4(regionName,"Emissions","Sec-"+name,gmap->first,"MTC",temp);
    }
    // CO2 emissions by Sector
    for ( int m=0;m<maxper;m++) {
        temp[m] = summary[m].get_emissmap_second("CO2");
    }
    dboutput4( regionName,"CO2 Emiss","by Sector",name,"MTC",temp);
    dboutput4( regionName,"CO2 Emiss",name,"zTotal","MTC",temp);

    // CO2 indirect emissions by Sector
    for ( int m=0;m<maxper;m++) {
        temp[m] = aIndEmissCalc->getIndirectEmissions( name, m );
    }
    dboutput4( regionName,"CO2 Emiss(ind)",name,"zTotal","MTC",temp);

    // Sector price
    for ( int m=0;m<maxper;m++) {
        temp[m] = getPrice( aGDP, m );
    }
    dboutput4( regionName,"Price",name,"zSectorAvg",mPriceUnit, temp );
    // for electricity Sector only
    if (name == "electricity") {
        for ( int m=0;m<maxper;m++) {
            temp[m] = getPrice( aGDP, m ) * 2.212 * 0.36;
        }
        dboutput4( regionName,"Price","electricity C/kWh","zSectorAvg","90C/kWh",temp);
    }

    // Sector price
    for ( int m = 0; m < maxper; m++ ) {
        temp[m] = getPrice( aGDP, m );
    }
    dboutput4( regionName,"Price","by Sector",name,mPriceUnit, temp );

    // do for all subsectors in the Sector
    for( int m = 0; m < maxper; m++ ) {
        temp[ m ] = getOutput( m );
    }

    for( unsigned int i = 0; i < subsec.size(); ++i ){
        // output or demand for each technology
        subsec[ i ]->MCoutputSupplySector( aGDP );
        subsec[ i ]->MCoutputAllSectors( aGDP, aIndEmissCalc, temp );
    }

    // do for all subsectors in the Sector
    for( unsigned int i = 0; i < subsec.size(); ++i ){
        // output or demand for each technology
        subsec[ i ]->csvOutputFile( aGDP, aIndEmissCalc );
    }
}

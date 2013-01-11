#!/usr/bin/env python3

import languagemodel
import syllables
import random

fives = """assimilation conscientiousness creativity diagnostician electricity
humiliation mathematical opportunity popularity similarity incredulity
pediatrician perpendicular unbelievable university vocabulary Accumulation
arbitrarily automatically beneficiary chemotherapy congratulations diagonally
evolutionist fiduciary geometrical hippopotamus indiscriminate justifiable
kaleidoscopic lasciviously monosyllabic neurological observatory perceptivity
qualification renunciation Snuffleupagus telepathically undeniable vociferously
water-diviner xenophobia Yugoslavian Zoroastrian""".split()

sevens = """inapplicability unsatisfactorily heterogeneity disintermediation
antiglobalization telecommunication interdisciplinary meteorological
socioeconomic intelligibility autobiographical industrialization epidemiologic
abiogenetical ateriosclerosis disproportionality editorializing artificiality
oversimplification intercolonization maneuverability conceptualization
decriminalization dephonologization infinitesimally superficiality
indestructability megalomaniacal sentimentalization fibrocartilaginous
proletarianism irrefutability eosinopenia paleobiology homoscedasticity
irreversibility paragonimiasis characteristicalness acanthopterygian
meningocephalitis orthostereoscopic representationism laryngoscopically
electrotherapeutics parallelepipedal galactopyranosic contemporaneity
aeropalynology anazonasulculate catazonasulculate anisodiametric meridionosulcus
striato-reticulate paleocarpology paleomycology paleoecology pseudointellectual
homosexuality irresponsibility antidiscrimination historiographical
hemopericardium hydrofluosilicic metallographically insolubilization
predispositionally electrodeposition unsystematically pseudepigraphically
patronomatology onomatophobia onomatomania odontotrichomelic tetraperomelia
craniosynostosis aluminosilicate paleocarpologist paleoecologist paleomycologist
pleuroperitonitis pleurocholecystitis pleuropericarditis pleuroperitoneal
electrolytically pluridisciplinary parallelopipedon internationalism
cytomegalovirus photophosphorylation hemianesthesia hyperpresbyopia
platystencephalia myxochondrosarcoma myringodermatitis myopericarditis
meningomalacia meningocerebritis karyomicrosoma malarimaxillary metasedimentary
jejunocolostomy periesophagitis pericardiorraphy pericholecystitis
peribronchiolitis infradiaphragmatic undistinguishableness indistinguishableness
indolaceturia incudostapedial actinopterygian incombustibility imperceptibility
incontestability insusceptibility unconformability galvanotherapeutics
galactometastasis chilostomatoplasty chlamydobacteria cephalopharyngeus
cephalorrhachidian cephalomeningitis cerebromalacia cerebromedullary
cervicoauricular alloisomerism allotriodontia allotriogeustia alveolosubnasal
prosopodiplegia prostatocystotomy pseudactinomycosis osteoarthropathy
pseudoakromegaly pseudoanorexia vesiculocavernous acromiothoracic
retroperitoneal proctosigmoidectomy proctoelytroplasty acardiohemia
acardionervia acardiotrophia acatamathesia acephalobrachia acephalorrhachia
acephalostomia achreocythemia hydroparacumaric acinetatrophia acousmatamnesia
acromiocoracoid acromiohumeral acroparesthesia actinodermatitis
acrotrophoneurosis acyanoblepsia adenocarcinoma superoxygenated tracheolaryngeal
genitourinary adrenalinemia aerodermectasia akidopeirastica alimentotherapy
anatherapeusis anisometropia anoceliadelphous anteroparietal thyrocartilaginous
viscerocutaneous omphalomesenteric omphalomesaraic denuclearization
abdominogenital abdominovesical abdominothoracic myoepithelial myoepithelium
myoneurasthenia myxofibrosarcoma cataleptolethargic magnetobiology
rheobiological sociodemographic photoplagiotropic diageotropism parthenogonidium
epipedochorisis hemiangiocarpic heterodichogamous teleologically
anthropomorphically mesenteriopexy phalangophalangeal ornithocoprophilous
colpohysteropexy paleoconservative pseudoaposematic individualistic
representationally sialoaerophagy mineralogically communicationally
combinatorically trigonometrically phantasmagorically sitosterolemia
phytosterolemia autodidactically insubstantialities neuroepithelial
neuroepithelium postcolonialism pharyngolaryngeal inconsequentially
unceremoniously antidopaminergic parapsychological electromechanical
ursodeoxycholic radiometrically impracticability septuagenarian
agathocacological impressionability platitudinarian hepatotoxicity
psychoacoustically geopolitically astronavigational encyclopedically
embryologically ontogenetically oncogenetically antihepadnaviral
parathyroidectomy homeostatically antagonistically counterintuitively
overconscientiously unidirectionally unsympathetically electromyography
biomimetically glycosaminoglycan anticipatorily contemporaneously
uncooperatively associativity revascularization intercommunicative
polygenetically peripatetically phytoremediating achronologically
parahydroxybenzene heredofamilial autocannibalism circumlocutorily
referentiality memorialization myelomonocytic megakaryocytic megakaryoblastic
lymphoepithelial micromyeloblastic leukocytogenesis albuminocholia
pylorogastrectomy nephroblastomatosis gerontotherapeutics zygomaticofacial
pyelointerstitial pupillostatometer esophagoplication enantiobiosis
unphilosophically pseudotuberculosis neurodevelopmental holoprosencephaly
ureidopenicillin paroemiographer noncatalytically heliographically
metalinguistically ideographically craniologically antiproliferation
tetrafluoroethylene topsyturvifications polarizability biholomorphically
photoactivatable anterohypophysis microcommunication inhomogeneity
enthusiastically echocardiographic physiognomically steatohepatitis
territoriality nucleocytoplasmic compositionality cardiometabolic
osteoprotegerin trimethoxytritanol plenipotentiary plenipotentiaries
spondyloarthropathy semiquantitatively nitrohydrocellulose carcinogenicity
phytohemagglutinin anachronistically docosahexaenoic atherogenicity
costicartilaginous cyclooctatetraene cyclotrigermenium metallacyclopentene
encyclopediacal cyclohexaamylose cyclomaltoheptaose epihypocycloidal
cycloelimination photocycloreversion retrocycloaddition macrobiostigmatic
brachybiostigmatic brachybiostigmatous apotropaically multijurisdictional
pallialinostima tetramethylrhodamine glycerophospholipid iridodialysis
parallelepipedoid uncommunicativeness perpendicularity unemotionality
metapragmatically tetrahydrogestrinone exopolysaccharide deoxycytidylate
antiferromagnetic levomethamphetamine syncategorematic categorematical
contraparallelogram methyldihydromorphine antiferrodistortion
antiferrodistortive cyclopentadienone intrapetiolary interpetiolary
geranylgeraniol ferrolyomesophase endosiphocylinder incogitativity
hypocraterimorphous subfunctionalization contradictoriously malacostracologist
soteriological librocubicularist anticyclogenesis methylthioninium
dichlorophenylphosphine bacteriopurpurin methyllycaconitine polyphthalocyanine
parachromatophorous antisabbatarian tiffanyite nordihydrocapsaicin
gastroesophageal mucocutaneous arteriosclerotic intersectionalism
incisodenticulate ballistocardiograph ballistocardiogram glaciofluviatile
tympanoperiotic deuterocanonical hypercuryprosopous
hyperleptoprosopous""".split()

def line_of_length(n, lm):
    """Generate a line of n syllables, using the given language model."""
    for attempt in range(100):
        out = []
        total = 0
        words = lm.generate(n)
        for word in words:
            out.append(word)
            total += syllables.count_syllables(word)
            if total == n:
                return " ".join(out).lower()
            if total > n:
                break
    print("WEIRD FAILURE")
    return random.choice(fives if (n == 5) else sevens)

def haiku_for(nick):
    one, two, three = None, None, None
    lm = languagemodel.language_model_for(nick)
    if lm:
        print(lm)
        one = line_of_length(5, lm)
        two = line_of_length(7, lm)
        three = line_of_length(5, lm)
    else:
        one = random.choice(fives)
        two = random.choice(sevens)
        three = random.choice(fives)
    return "{0} / {1} / {2}".format(one, two, three)

def main():
    print(haiku_for("alexr"))

if __name__ == "__main__": main()

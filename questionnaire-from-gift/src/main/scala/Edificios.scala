import scala.util.Random

/**
 * Created by alvaro on 21/07/15.
 * Simulacion del problema
 * https://www.facebook.com/EPM.MigueldeGuzman/photos/a.212386218829910.47150.198035636931635/833727006695825/?type=1
 */
object Edificios extends App{

  import Math.pow

  case class Altura( altura: Int )

  implicit def alturaAInt( a: Altura ) = a.altura

  def colorAleatorio() = Random.nextInt(2)

  def vemosLosDosColores( edificios: Int ) = {
    val primerColor = colorAleatorio()
    val diferente = (1 to edificios-1).find( _ => colorAleatorio() != primerColor )
    diferente.nonEmpty
  }

  def probabilidadDeVerLosDosColoresConExperimentos( veces: Int = 100 )( edificios: Int) = {
    ( 1 to veces ).map( _ => vemosLosDosColores(edificios) ).count( b => b ).toDouble / veces
  }

  def probabilidadDeQueSeanDelMismoColor( edificios: Int ) = 1.0/pow(2,edificios-1)

  def probabilidadDeVerLosDosColoresConMatematicas( edificios: Int ) = 1 - probabilidadDeQueSeanDelMismoColor(edificios)

  val probabilidadDeVerLosDosColores = probabilidadDeVerLosDosColoresConMatematicas _

  def alturaAleatoria( implicit max: Altura ) = Random.nextInt(max+1)

  def alturasAleatorias(implicit max: Altura ) = Iterator.continually(alturaAleatoria(max))

  def contarEdificiosHastaQueLaAlturaSeaMayorQue( alturaInicial: Int)(implicit alturaMaxima: Altura  ) = {
    alturasAleatorias.indexWhere( _ >= alturaInicial ) + 1
  }

  def unExperimento( implicit alturaMaxima: Altura ) : Double = {
    val alturaEdificioPropio = alturaAleatoria

    if( alturaEdificioPropio == alturaMaxima.altura ){
      // SI ES LA ALTURA MAXIMA, SEGURO QUE VEMOS TODOS LOS INFINITOS EDIFICIOS
      return 1.0
    }

    val visiblesPorLaDerecha = contarEdificiosHastaQueLaAlturaSeaMayorQue(alturaEdificioPropio) ensuring (_ >= 1)
    val visiblesPorLaIzquierda = contarEdificiosHastaQueLaAlturaSeaMayorQue(alturaEdificioPropio) ensuring (_ >= 1)

    probabilidadDeVerLosDosColores(visiblesPorLaDerecha + visiblesPorLaIzquierda) ensuring ( r => r >= 0 && r <= 1 )
  }

  def experimento( veces: Int)(implicit alturaMaxima: Altura ) = {
    (1 to veces ).map( _ => unExperimento ).sum  / veces
  }


  val veces = 100000
  for( a <- 1 to 200 ) {
    print( s"altura máxima:$a\t resultado:${experimento(veces)(Altura(a))}\n" )
  }

  /*
altura máxima:1	 resultado:0.7485
altura máxima:2	 resultado:0.726804833984375
altura máxima:3	 resultado:0.726891310005188
altura máxima:4	 resultado:0.7316911618280411
altura máxima:5	 resultado:0.7363076835475862
altura máxima:6	 resultado:0.7403322324729299
altura máxima:7	 resultado:0.7438657317095198
altura máxima:8	 resultado:0.7454309354877481
altura máxima:9	 resultado:0.749177747942666
altura máxima:10	 resultado:0.7508121394341465
altura máxima:11	 resultado:0.7510852049227655
altura máxima:12	 resultado:0.7545703868665986
altura máxima:13	 resultado:0.7541762438251829
altura máxima:14	 resultado:0.7559886297161107
altura máxima:15	 resultado:0.7572315411442567
altura máxima:16	 resultado:0.7572590680871176
altura máxima:17	 resultado:0.7603458307320736
altura máxima:18	 resultado:0.7595840508780742
altura máxima:19	 resultado:0.76064550766025
altura máxima:20	 resultado:0.7609108987498915
altura máxima:21	 resultado:0.7618274032077224
altura máxima:22	 resultado:0.7613019351571506
altura máxima:23	 resultado:0.7621058366085234
altura máxima:24	 resultado:0.7626899093151186
altura máxima:25	 resultado:0.7627124719233707
altura máxima:26	 resultado:0.7627653411855769
altura máxima:27	 resultado:0.7638287843438526
altura máxima:28	 resultado:0.7645906834985978
altura máxima:29	 resultado:0.7656765986728694
altura máxima:30	 resultado:0.7653256821569795
altura máxima:31	 resultado:0.7653186075356379
altura máxima:32	 resultado:0.7654926929639841
altura máxima:33	 resultado:0.7654746031288261
altura máxima:34	 resultado:0.7659992833328427
altura máxima:35	 resultado:0.7655763031846696
altura máxima:36	 resultado:0.7673584188980883
altura máxima:37	 resultado:0.7665369585449825
altura máxima:38	 resultado:0.766266366579463
altura máxima:39	 resultado:0.7670974583145677
altura máxima:40	 resultado:0.7661577802135068
altura máxima:41	 resultado:0.7669159103779384
altura máxima:42	 resultado:0.7661964977740409
altura máxima:43	 resultado:0.7669399198949018
altura máxima:44	 resultado:0.7665093159268911
altura máxima:45	 resultado:0.7665666774563616
altura máxima:46	 resultado:0.7670948757355495
altura máxima:47	 resultado:0.7673242252955277
altura máxima:48	 resultado:0.7667809906821832
altura máxima:49	 resultado:0.7674714598371254
altura máxima:50	 resultado:0.7671780530949438
altura máxima:51	 resultado:0.7673237197983059
altura máxima:52	 resultado:0.767662549613083
altura máxima:53	 resultado:0.7680708380754164
altura máxima:54	 resultado:0.7684615121641195
altura máxima:55	 resultado:0.7680306808704147
altura máxima:56	 resultado:0.7675570807697537
altura máxima:57	 resultado:0.7696915424414355
altura máxima:58	 resultado:0.7678450735074949
altura máxima:59	 resultado:0.7689853235831396
altura máxima:60	 resultado:0.769641933018672
altura máxima:61	 resultado:0.7685774175043787
altura máxima:62	 resultado:0.7695217957295285
altura máxima:63	 resultado:0.7692075092293252
altura máxima:64	 resultado:0.7682346844971973
altura máxima:65	 resultado:0.7682748420200333
altura máxima:66	 resultado:0.7684413774224892
altura máxima:67	 resultado:0.7695991176312542
altura máxima:68	 resultado:0.7700810029780424
altura máxima:69	 resultado:0.76829892550096
altura máxima:70	 resultado:0.7678627584056581
altura máxima:71	 resultado:0.7681328531831276
altura máxima:72	 resultado:0.7682338593959502
altura máxima:73	 resultado:0.7692929890325607
altura máxima:74	 resultado:0.7686993883920302
altura máxima:75	 resultado:0.7695345562479071
altura máxima:76	 resultado:0.7692379051248701
altura máxima:77	 resultado:0.7688060024780132
altura máxima:78	 resultado:0.7687476117099128
altura máxima:79	 resultado:0.7687219019057475
altura máxima:80	 resultado:0.7695327221063681
altura máxima:81	 resultado:0.7685082068188043
altura máxima:82	 resultado:0.7697571987249658
altura máxima:83	 resultado:0.7693073845232257
altura máxima:84	 resultado:0.7693305993581523
altura máxima:85	 resultado:0.7704406652509844
altura máxima:86	 resultado:0.7692179971923564
altura máxima:87	 resultado:0.7697398375811219
altura máxima:88	 resultado:0.7703790082376938
altura máxima:89	 resultado:0.7699152811218686
altura máxima:90	 resultado:0.7695555094284245
altura máxima:91	 resultado:0.7707705944618817
altura máxima:92	 resultado:0.7699176629085935
altura máxima:93	 resultado:0.7698993787624445
altura máxima:94	 resultado:0.7692250977611479
altura máxima:95	 resultado:0.7699597114547485
altura máxima:96	 resultado:0.7701589266422006
altura máxima:97	 resultado:0.7702671042214008
altura máxima:98	 resultado:0.7714649746555846
altura máxima:99	 resultado:0.7695164238160921
altura máxima:100	 resultado:0.7692816942077283
altura máxima:101	 resultado:0.7702927950130412
altura máxima:102	 resultado:0.7698558997833194
altura máxima:103	 resultado:0.7708689491016791
altura máxima:104	 resultado:0.7697865050313444
altura máxima:105	 resultado:0.7694554868784477
altura máxima:106	 resultado:0.7705443854304732
altura máxima:107	 resultado:0.769664438248511
altura máxima:108	 resultado:0.7695355863804513
altura máxima:109	 resultado:0.770850407548487
altura máxima:110	 resultado:0.7702032715851186
altura máxima:111	 resultado:0.7709741243469648
altura máxima:112	 resultado:0.7693645748495492
altura máxima:113	 resultado:0.7709781235169808
altura máxima:114	 resultado:0.7694280837549748
altura máxima:115	 resultado:0.7703998290783537
altura máxima:116	 resultado:0.7694979359361928
altura máxima:117	 resultado:0.7690430921102107
altura máxima:118	 resultado:0.7699348946228469
altura máxima:119	 resultado:0.7693596391161232
altura máxima:120	 resultado:0.7711421861918477
altura máxima:121	 resultado:0.769282293244441
altura máxima:122	 resultado:0.7708980721376624
altura máxima:123	 resultado:0.7719205111970846
altura máxima:124	 resultado:0.7706405184933254
altura máxima:125	 resultado:0.7706082924420257
altura máxima:126	 resultado:0.7717804670428765
altura máxima:127	 resultado:0.7710228262944668
altura máxima:128	 resultado:0.7697457161344157
altura máxima:129	 resultado:0.7698375603754493
altura máxima:130	 resultado:0.7700743706119189
altura máxima:131	 resultado:0.7720365615883187
altura máxima:132	 resultado:0.7700868211733163
altura máxima:133	 resultado:0.7701226565341535
altura máxima:134	 resultado:0.7708411317966666
altura máxima:135	 resultado:0.7697972674896952
altura máxima:136	 resultado:0.770951546939308
altura máxima:137	 resultado:0.7707756484907925
altura máxima:138	 resultado:0.7714245039245385
altura máxima:139	 resultado:0.7708630444287744
altura máxima:140	 resultado:0.7703182877884742
altura máxima:141	 resultado:0.7704219001647294
altura máxima:142	 resultado:0.7719279022286312
altura máxima:143	 resultado:0.771265789371338
altura máxima:144	 resultado:0.7711843833378429
altura máxima:145	 resultado:0.7709687197741972
altura máxima:146	 resultado:0.7706994203268478
altura máxima:147	 resultado:0.7708523057492359
altura máxima:148	 resultado:0.7718840083562512
altura máxima:149	 resultado:0.7709547287853349
altura máxima:150	 resultado:0.7731732481689798
altura máxima:151	 resultado:0.7712646730225958
altura máxima:152	 resultado:0.7714730588987357
altura máxima:153	 resultado:0.7710903430720824
altura máxima:154	 resultado:0.7697686091781588
altura máxima:155	 resultado:0.7715993264170984
altura máxima:156	 resultado:0.7706046996407014
altura máxima:157	 resultado:0.7714724665412496
altura máxima:158	 resultado:0.7720889112283105
altura máxima:159	 resultado:0.7706035454996464
altura máxima:160	 resultado:0.7710661798549558
altura máxima:161	 resultado:0.7717515328189316
altura máxima:162	 resultado:0.7702575072104244
altura máxima:163	 resultado:0.770760214188431
altura máxima:164	 resultado:0.77088893590659
altura máxima:165	 resultado:0.7722422532872173
altura máxima:166	 resultado:0.7711561529182736
altura máxima:167	 resultado:0.7711290876464537
altura máxima:168	 resultado:0.7705298170616676
altura máxima:169	 resultado:0.7699341288119809
altura máxima:170	 resultado:0.7711839780355452
altura máxima:171	 resultado:0.7706237300180161
altura máxima:172	 resultado:0.7717949101018332
altura máxima:173	 resultado:0.7707512152160914
altura máxima:174	 resultado:0.7715646397000937
altura máxima:175	 resultado:0.7704361069635974
altura máxima:176	 resultado:0.7705175609918339
altura máxima:177	 resultado:0.771480924030609
altura máxima:178	 resultado:0.7714843999562753
altura máxima:179	 resultado:0.7711834628946459
altura máxima:180	 resultado:0.7713133254847444
altura máxima:181	 resultado:0.7704747848955248
altura máxima:182	 resultado:0.7718030602137308
altura máxima:183	 resultado:0.7708612497682036
altura máxima:184	 resultado:0.7708563893607614
altura máxima:185	 resultado:0.771249879197368
altura máxima:186	 resultado:0.7722100104882399
altura máxima:187	 resultado:0.7718170140668754
altura máxima:188	 resultado:0.7707792164336106
altura máxima:189	 resultado:0.770841690981244
altura máxima:190	 resultado:0.7700511634232319
altura máxima:191	 resultado:0.7710172769395232
altura máxima:192	 resultado:0.7712627626752613
altura máxima:193	 resultado:0.7712468544763126
altura máxima:194	 resultado:0.7718619938672429
altura máxima:195	 resultado:0.7722180984206053
altura máxima:196	 resultado:0.7712667963837848
altura máxima:197	 resultado:0.7705378807555257
altura máxima:198	 resultado:0.7706229466674881
altura máxima:199	 resultado:0.772162627739415
altura máxima:200	 resultado:0.7721868049627015

   */
}

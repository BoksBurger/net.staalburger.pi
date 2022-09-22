function onLoad() {
  document.addEventListener("deviceready", onDeviceReady, false);
}

function onDeviceReady() {
  main();
  console.log("object");
}

$(document).on("pagecreate", "#amplePi", function(e) {
  $(document).on("swiperight", "#amplePi", function(e) {
    if ($(".ui-page-active").jqmData("panel") !== "open") {
      if (e.type === "swiperight") {
        $(".panel").panel("open");
      }
    }
  });
});

$(document).on("swiperight", ".nav-back", function(e) {
  window.history.back();
});

$(document).on("swipeleft", ".nav-back, #amplePi", function(e) {
  window.history.forward();
});

let x =
  "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679821480865132823066470938446095505822317253594081284811174502841027019385211055596446229489549303819644288109756659334461284756482337867831652712019091456485669234603486104543266482133936072602491412737245870066063155881748815209209628292540917153643678925903600113305305488204665213841469519415116094330572703657595919530921861173819326117931051185480744623799627495673518857527248912279381830119491298336733624406566430860213949463952247371907021798609437027705392171762931767523846748184676694051320005681271452635608277857713427577896091736371787214684409012249534301465495853710507922796892589235420199561121290219608640344181598136297747713099605187072113499999983729780499510597317328160963185950244594553469083026425223082533446850352619311881710100031378387528865875332083814206171776691473035982534904287554687311595628638823537875937519577818577805321712268066130019278766111959092164201989380952572010654858632788659361533818279682303019520353018529689957736225994138912497217752834791315155748572424541506959508295331168617278558890750983817546374649393192550604009277016711390098488240128583616035637076601047101819429555961989467678374494482553797747268471040475346462080466842590694912933136770289891521047521620569660240580381501935112533824300355876402474964732639141992726042699227967823547816360093417216412199245863150302861829745557067498385054945885869269956909272107975093029553211653449872027559602364806654991198818347977535663698074265425278625518184175746728909777727938000816470600161452491921732172147723501414419735685481613611573525521334757418494684385233239073941433345477624168625189835694855620992192221842725502542568876717904946016534668049886272327917860857843838279679766814541009538837863609506800642251252051173929848960841284886269456042419652850222106611863067442786220391949450471237137869609563643719172874677646575739624138908658326459958133904780275900994657640789512694683983525957098258226205224894077267194782684826014769909026401363944374553050682034962524517493996514314298091906592509372216964615157098583874105978859597729754989301617539284681382686838689427741559918559252459539594310499725246808459872736446958486538367362226260991246080512438843904512441365497627807977156914359977001296160894416948685558484063534220722258284886481584560285060168427394522674676788952521385225499546667278239864565961163548862305774564980355936345681743241125150760694794510965960940252288797108931456691368672287489405601015033086179286809208747609178249385890097149096759852613655497818931297848216829989487226588048575640142704775551323796414515237462343645428584447952658678210511413547357395231134271661021359695362314429524849371871101457654035902799344037420073105785390621983874478084784896833214457138687519435064302184531910484810053706146806749192781911979399520614196634287544406437451237181921799983910159195618146751426912397489409071864942319615679452080951465502252316038819301420937621378559566389377870830390697920773467221825625996615014215030680384477345492026054146659252014974428507325186660021324340881907104863317346496514539057962685610055081066587969981635747363840525714591028970641401109712062804390397595156771577004203378699360072305587631763594218731251471205329281918261861258673215791984148488291644706095752706957220917567116722910981690915280173506712748583222871835209353965725121083579151369882091444210067510334671103141267111369908658516398315019701651511685171437657618351556508849099898599823873455283316355076479185358932261854896321329330898570642046752590709154814165498594616371802709819943099244889575712828905923233260972997120844335732654893823911932597463667305836041428138830320382490375898524374417029132765618093773444030707469211201913020330380197621101100449293215160842444859637669838952286847831235526582131449576857262433441893039686426243410773226978028073189154411010446823252716201052652272111660396665573092547110557853763466820653109896526918620564769312570586356620185581007293606598764861179104533488503461136576867532494416680396265797877185560845529654126654085306143444318586769751456614068007002378776591344017127494704205622305389945613140711270004078547332699390814546646458807972708266830634328587856983052358089330657574067954571637752542021149557615814002501262285941302164715509792592309907965473761255176567513575178296664547791745011299614890304639947132962107340437518957359614589019389713111790429782856475032031986915140287080859904801094121472213179476477726224142548545403321571853061422881375850430633217518297986622371721591607716692547487389866549494501146540628433663937900397692656721463853067360965712091807638327166416274888800786925602902284721040317211860820419000422966171196377921337575114959501566049631862947265473642523081770367515906735023507283540567040386743513622224771589150495309844489333096340878076932599397805419341447377441842631298608099888687413260472156951623965864573021631598193195167353812974167729478672422924654366800980676928238280689964004824354037014163149658979409243237896907069779422362508221688957383798623001593776471651228935786015881617557829735233446042815126272037343146531977774160319906655418763979293344195215413418994854447345673831624993419131814809277771038638773431772075456545322077709212019051660962804909263601975988281613323166636528619326686336062735676303544776280350450777235547105859548702790814356240145171806246436267945612753181340783303362542327839449753824372058353114771199260638133467768796959703098339130771098704085913374641442822772634659470474587847787201927715280731767907707157213444730605700733492436931138350493163128404251219256517980694113528013147013047816437885185290928545201165839341965621349143415956258658655705526904965209858033850722426482939728584783163057777560688876446248246857926039535277348030480290058760758251047470916439613626760449256274204208320856611906254543372131535958450687724602901618766795240616342522577195429162991930645537799140373404328752628889639958794757291746426357455254079091451357111369410911939325191076020825202618798531887705842972591677813149699009019211697173727847684726860849003377024242916513005005168323364350389517029893922334517220138128069650117844087451960121228599371623130171144484640903890644954440061986907548516026327505298349187407866808818338510228334508504860825039302133219715518430635455007668282949304137765527939751754613953984683393638304746119966538581538420568533862186725233402830871123282789212507712629463229563989898935821167456270102183564622013496715188190973038119800497340723961036854066431939509790190699639552453005450580685501956730229219139339185680344903982059551002263535361920419947455385938102343955449597783779023742161727111723643435439478221818528624085140066604433258885698670543154706965747458550332323342107301545940516553790686627333799585115625784322988273723198987571415957811196358330059408730681216028764962867446047746491599505497374256269010490377819868359381465741268049256487985561453723478673303904688383436346553794986419270563872931748723320837601123029911367938627089438799362016295154133714248928307220126901475466847653576164773794675200490757155527819653621323926406160136358155907422020203187277605277219005561484255518792530343513984425322341576233610642506390497500865627109535919465897514131034822769306247435363256916078154781811528436679570611086153315044521274739245449454236828860613408414863776700961207151249140430272538607648236341433462351897576645216413767969031495019108575984423919862916421939949072362346468441173940326591840443780513338945257423995082965912285085558215725031071257012668302402929525220118726767562204154205161841634847565169998116141010029960783869092916030288400269104140792886215078424516709087000699282120660418371806535567252532567532861291042487761825829765157959847035622262934860034158722980534989650226291748788202734209222245339856264766914905562842503912757710284027998066365825488926488025456610172967026640765590429099456815065265305371829412703369313785178609040708667114965583434347693385781711386455873678123014587687126603489139095620099393610310291616152881384379099042317473363948045759314931405297634757481193567091101377517210080315590248530906692037671922033229094334676851422144773793937517034436619910403375111735471918550464490263655128162288244625759163330391072253837421821408835086573917715096828874782656995995744906617583441375223970968340800535598491754173818839994469748676265516582765848358845314277568790029095170283529716344562129640435231176006651012412006597558512761785838292041974844236080071930457618932349229279650198751872127267507981255470958904556357921221033346697499235630254947802490114195212382815309114079073860251522742995818072471625916685451333123948049470791191532673430282441860414263639548000448002670496248201792896476697583183271314251702969234889627668440323260927524960357996469256504936818360900323809293459588970695365349406034021665443755890045632882250545255640564482465151875471196218443965825337543885690941130315095261793780029741207665147939425902989695946995565761218656196733786236256125216320862869222103274889218654364802296780705765615144632046927906821207388377814233562823608963208068222468012248261177185896381409183903673672220888321513755600372798394004152970028783076670944474560134556417254370906979396122571429894671543578468788614445812314593571984922528471605049221242470141214780573455105008019086996033027634787081081754501193071412233908663938339529425786905076431006383519834389341596131854347546495569781038293097164651438407007073604112373599843452251610507027056235266012764848308407611830130527932054274628654036036745328651057065874882256981579367897669742205750596834408697350201410206723585020072452256326513410559240190274216248439140359989535394590944070469120914093870012645600162374288021092764579310657922955249887275846101264836999892256959688159205600101655256375678";
let p = 4;
let strike = 0;
let streak = 0;
let level = 0;
let struck = false;
let scrollingPaused = false;
let scrollingPi = false;
let enterPosition = false;
let enteredPosition = 0;
let inconsistencyText = "<hr />Inconsistencies:";
//Search
let searchingPi = false;
let searchingNextPi = false;
let searchDisplayCurrentPi = 0;
let searchCurrentPosition = 0;
let searchValue = "";
let searchOccurrences = 0;
let searchReset = false;
// End search
let enableZen = false;
let enableTimer = false;
let timerStarted = false;
// Settings defaults
let showHints = 3;
let strikeReset = 3;
let countDownTime = 30;
let errorHint = false;
let piMaster = true; //No hints taken.
let strikeOutReset = false;
let scrollingAutoStart = false;
let groupDigitsEvery = 4;
let useAlternateKeypad = false;
$(document).on("pageinit", function() {
  $.extend($.mobile, {
    defaultPageTransition: "fade"
  });
});

function saveSettings(loadDefaults) {
  if (loadDefaults) {
    groupDigitsEvery = 4;
    showHints = 3;
    strikeReset = 3;
    countDownTime = 30;
    errorHint = false;
    strikeOutReset = false;
    scrollingAutoStart = false;
    useAlternateKeypad = false;
  } else {
    showHints = $("#showHints").val();
    strikeReset = $("#strikeReset").val();
    countDownTime = $("#countDownTime").val();
    errorHint = $("#errorHint").prop("checked");
    strikeOutReset = $("#strikeOutReset").prop("checked");
    scrollingAutoStart = $("#scrollingAutoStart").prop("checked");
    useAlternateKeypad = $("#useAlternateKeypad").prop("checked");
    groupDigitsEvery = $("#groupDigitsEvery").val();
  }
  localStorage.setItem("showHints", showHints);
  localStorage.setItem("strikeReset", strikeReset);
  localStorage.setItem("countDownTime", countDownTime);
  localStorage.setItem("errorHint", errorHint);
  localStorage.setItem("strikeOutReset", strikeOutReset);
  localStorage.setItem("groupDigitsEvery", groupDigitsEvery);
  localStorage.setItem("scrollingAutoStart", scrollingAutoStart);
  localStorage.setItem("useAlternateKeypad", useAlternateKeypad);
  loadSettings(true);
  if (enableTimer) {
    reset();
  }
}

function loadSettings(refresh) {
  if (localStorage["showHints"] != null) {
    showHints = parseInt(localStorage["showHints"]);
    strikeReset = parseInt(localStorage["strikeReset"]);
    countDownTime = parseInt(localStorage["countDownTime"]);
    errorHint = localStorage["errorHint"] === "true";
    strikeOutReset = localStorage["strikeOutReset"] === "true";
    scrollingAutoStart = localStorage["scrollingAutoStart"] === "true";
    useAlternateKeypad = localStorage["useAlternateKeypad"] === "true";
    groupDigitsEvery = parseInt(localStorage["groupDigitsEvery"]);
  } else {
    localStorage.setItem("showHints", showHints);
    localStorage.setItem("strikeReset", strikeReset);
    localStorage.setItem("countDownTime", countDownTime);
    localStorage.setItem("errorHint", errorHint);
    localStorage.setItem("strikeOutReset", strikeOutReset);
    localStorage.setItem("groupDigitsEvery", groupDigitsEvery);
    localStorage.setItem("scrollingAutoStart", scrollingAutoStart);
    localStorage.setItem("useAlternateKeypad", useAlternateKeypad);
  }
  if (refresh) {
    $("#showHints")
      .val(showHints)
      .slider("refresh");
    $("#strikeReset")
      .val(strikeReset)
      .slider("refresh");
    $("#countDownTime")
      .val(countDownTime)
      .slider("refresh");
    $("#groupDigitsEvery")
      .val(groupDigitsEvery)
      .slider("refresh");
    $("#errorHint")
      .prop("checked", errorHint)
      .flipswitch("refresh");
    $("#strikeOutReset")
      .prop("checked", strikeOutReset)
      .flipswitch("refresh");
    $("#scrollingAutoStart")
      .prop("checked", scrollingAutoStart)
      .flipswitch("refresh");
    $("#useAlternateKeypad")
      .prop("checked", useAlternateKeypad)
      .flipswitch("refresh");
  }
}

function main() {
  loadSettings(false);
  setKeypad(useAlternateKeypad);
  reset();
  $("#showMenu").on("click", e => {
    $("#left-panel").panel("open");
  });
  $("#amplePi").on("pageshow", function() {
    setKeypad(useAlternateKeypad);
  });
  $("#ampleSettings").on("pageshow", function() {
    loadSettings(true);
  });
  $("#piDayCountDownTimer").on("pagebeforeshow", function() {
    $("#resetPiCountdownTimer").hide();
    let piDate = "03-14";
    let piYear = new Date().getFullYear();
    if (localStorage.getItem("nextPiYear")) {
      piYear = localStorage.getItem("nextPiYear");
    }
    let deadline = new Date(`${piYear}-${piDate}`);
    //let deadline = new Date(Date.parse(new Date()) + 1 * 1 * 1 * 3 * 1000); //Test
    initializeClock("clockdiv", deadline);
  });
  $("#resetPiCountdownTimer").on("click", function() {
    let nextPiYear = new Date().getFullYear() + 1;
    localStorage.setItem("nextPiYear", nextPiYear.toString());
    let piDate = "03-14";
    let deadline = new Date(`${nextPiYear}-${piDate}`);
    //let deadline = new Date(Date.parse(new Date()) + 1 * 1 * 1 * 3 * 1000); //Test
    $("#resetPiCountdownTimer").hide();
    $("#clockHead").html(
      `<span style="font-size:large;">Counting down to:</span><br /> <span style="font-size:x-large;">Pi day ${nextPiYear}</span>`
    );
    initializeClock("clockdiv", deadline);
  });
  $("#saveSettings").on("click", function() {
    saveSettings(false);
    navigator.vibrate(250);
    $.mobile.back();
  });
  $("#defaultSettings").on("click", function() {
    saveSettings(true);
    navigator.vibrate(100);
  });
  $("#resetScore").on("click", function() {
    localStorage.setItem("level", "0");
    localStorage.setItem("highScore", "0");
    $("#score").html("0");
    $("#level").html("0");
    $("#rec").html("0");
    strike = 0;
    streak = 0;
    level = 0;
    navigator.vibrate(200);
    $.mobile.back();
  });
  $("#timerNo").on("click", function() {
    $("#popupMessage").popup("close");
    enableTimer = false;
    $("#timePi").html("Enable timer");
    reset();
  });
  $("#timerYes").on("click", function() {
    $("#popupMessage").popup("close");
    reset();
  });
  bindKeypad();
  $("#reset, #xreset").on("click", function() {
    if (
      $("#pi")
        .html()
        .search("Pos") === 0
    ) {
      $("#pi").html("Enter position...");
    } else if (searchingPi) {
      if (!searchReset) {
        resetSearch();
        navigator.vibrate(50);
      } else {
        reset();
        navigator.vibrate([50, 50, 50, 50, 50]);
      }
    } else {
      reset();
      navigator.vibrate([50, 50, 50, 50, 50]);
    }
  });
  $("#loadLevel").on("click", function() {
    loadCheckPoint();
  });
  $("#pi").on("click", function() {
    onClickVibrate(100);
    if (scrollingPaused) {
      startScrolling();
      scrollingPaused = false;
    } else if (typeof piScroller != "undefined") {
      clearInterval(piScroller);
      delete piScroller;
      scrollingPaused = true;
    }
    let pos = parseInt(enteredPosition);
    if (pos > 0) {
      scrollControl(pos - 1);
    }
    if (searchingNextPi) {
      if (searchDisplayCurrentPi > searchOccurrences) {
        searchReset();
      } else {
        showLoader("Retreiving next position...");
        $.post("http://www.staalburger.net/apps/pi.php", {
          qq: searchValue,
          qqss: (parseInt(searchCurrentPosition) + 1).toString()
        }).done(function(data) {
          //console.log(data);
          hideLoader();
          if (data.r[0].status == "found") {
            searchCurrentPosition = data.r[0].p;
            searchOccurrences = parseInt(data.r[0].c);
            searchDisplayCurrentPi++;
            let occurranceText = "";
            if (searchOccurrences > 0) {
              if (searchOccurrences == searchDisplayCurrentPi) {
                occurranceText = "<br />Thats it. Tap again to start over.";
                searchingNextPi = false;
              } else {
                occurranceText =
                  "<br />Displaying " +
                  searchDisplayCurrentPi +
                  " of " +
                  searchOccurrences +
                  "<br /> Tap again to find next";
              }
            }
            let found =
              "Found: " +
              searchValue +
              "<br /><span style='font-size: large;'>occurs at position " +
              searchCurrentPosition +
              occurranceText +
              "</span>";
            $("#pi").html(found);
          } else {
            $("#pi").html(
              "Not found: " +
                searchValue +
                "<br />(Hit π to search again.)</span>"
            );
          }
        });
      }
    } else if (searchingPi) {
      if (searchValue != "") {
        showLoader("Searching pi...");
        $.post("http://www.staalburger.net/apps/pi.php", {
          q: searchValue
        })
          .done(function(data) {
            //console.log(data);
            hideLoader();
            if (data.r[0].status == "found") {
              searchCurrentPosition = data.r[0].p;
              searchOccurrences = parseInt(data.r[0].c);
              searchDisplayCurrentPi = 1;
              searchingNextPi = true;
              let occurranceText = "";
              if (searchOccurrences > 0) {
                occurranceText =
                  "<br />Displaying " +
                  searchDisplayCurrentPi +
                  " of " +
                  searchOccurrences +
                  "<br /> Tap again to find next";
              }
              let found =
                "Found: " +
                searchValue +
                "<br /><span style='font-size: large;'>occurs at position " +
                searchCurrentPosition +
                occurranceText +
                "</span>";
              $("#pi").html(found);
            }
          })
          .fail(function(jqXHR) {
            $("#pi").html("Service unavailable. Please check your connection.");
            hideLoader();
          });
      }
    }
  });
  $("#scrollPi").on("click", function() {
    scrollControl(0);
  });
  $("#scrollPiLevel").on("click", function() {
    scrollControl(parseInt(level + "0"));
  });
  $("#scrollPiPosition").on("click", function() {
    reset();
    enterPosition = true;
    $("#left-panel").panel("close");
    $("#pi").html("Enter position...");
  });
  $("#searchPi").on("click", function() {
    reset();
    searchingPi = true;
    $("#left-panel").panel("close");
    $("#pi").html("Search Pi...");
  });
  $("#calculatePi").on("click", function(e) {
    e.preventDefault();
    $.mobile.navigate("#piCalculation");
  });
  $("#zenPi").on("click", function() {
    reset();
    enableZen = !enableZen;
    zenMode(enableZen);
  });
  $("#timePi").on("click", function() {
    reset();
    enableTimer = !enableTimer;
    toggleTimer(enableTimer);
  });
  $("#piDayCountDown, #ampleTimer").on("click", function(e) {
    e.preventDefault();
    $.mobile.navigate("#piDayCountDownTimer");
  });
  $("#aboutPi").on("click", function(e) {
    e.preventDefault();
    $.mobile.navigate("#aboutAmplePi");
  });
  $("#settingsPi").on("click", function(e) {
    e.preventDefault();
    $.mobile.navigate("#ampleSettings");
  });
  $(".vibraClick").on("click", function() {
    onClickVibrate(10);
  });
}

function setKeypad(useAlternateKeypad) {
  if (useAlternateKeypad) {
    $("#altKeyPad").removeClass("hide");
    $("#keyPad").addClass("hide");
  } else {
    $("#altKeyPad").addClass("hide");
    $("#keyPad").removeClass("hide");
  }
}

function onClickVibrate(i) {
  navigator.vibrate(i);
}

function toggleTimer(timer) {
  if (timer) {
    $("#timePi").html("Disable timer");
    timerStarted = false;
    $("#pi").html(
      "3.14...<br /><span style='font-size: large;'>(Timer starts on first digit.)</span>"
    );
  } else {
    resetTimer(true);
    $("#timePi").html("Enable timer");
    timerStarted = false;
    if (enableZen) {
      $("#pi").html(
        "3.14...<br /><span style='font-size: large;'>(Now focus!)</span>"
      );
    } else {
      $("#pi").html(
        "3.14...<br /><span style='font-size: large;'>(Timer disabled.)</span>"
      );
    }
  }
}

function resetTimer(clear) {
  if (clear) {
    $("#piTimer").pietimer("pause");
    delete $("#piTimer").pietimer();
    piTimer();
    $(".ui-block-b.live").removeClass("zen");
  } else {
    $("#piTimer").pietimer("pause");
  }
}

function startTimer() {
  $("#piTimer").pietimer("start");
  $(".ui-block-b.live").addClass("zen");
  timerStarted = true;
}

function piTimer() {
  $("#piTimer").pietimer(
    {
      seconds: countDownTime,
      color: "#56838c87",
      height: 60,
      width: 60
    },
    function() {
      $("#popupMessage h1").html("Time's up!");
      $(".ui-block-b.live").removeClass("zen");
      $("#popupMessage").popup("open");
    }
  );
  //console.log(countDownTime);
  //$('#piTimer').pietimer('start');
}

function zenMode(zen) {
  if (zen) {
    $("#mode").addClass("zen", 1000);
    $(".ui-panel").addClass("ui-page-override", 1000);
    $(".ui-panel-wrapper").addClass("ui-page-override", 1000);
    $("#zenPi").html("Exit focus mode", 1000);
    $("#pi").addClass("zenPi", 1000);
    $(".key").addClass("zenKey", 1000);
    $(".altKey").addClass("zenKey", 1000);
    $(".left-panel-menu").addClass("left-panel-menu-zen", 1000);
    $("#showMenu").addClass("zenMenu", 1000);
    enableTimer = false;
    toggleTimer(enableTimer);
  } else {
    $("#mode").removeClass("zen", 1000);
    $(".ui-panel").removeClass("ui-page-override", 1000);
    $(".ui-panel-wrapper").removeClass("ui-page-override", 1000);
    $("#zenPi").html("Focus mode", 1000);
    $("#pi").removeClass("zenPi", 1000);
    $(".key").removeClass("zenKey", 1000);
    $(".altKey").removeClass("zenKey", 1000);
    $(".left-panel-menu").removeClass("left-panel-menu-zen", 1000);
  }
}

function scrollControl(position) {
  reset();
  if (scrollingPaused) {
    startScrolling();
    scrollingPaused = false;
  } else if (typeof piScroller != "undefined") {
    clearInterval(piScroller);
    delete piScroller;
    scrollingPaused = true;
  } else if (typeof piScroller == "undefined") {
    startPiScroller(scrollingAutoStart, position);
  }
}

function startPiScroller(autoStart, position) {
  let groupsOf = parseInt(localStorage["groupDigitsEvery"]);
  let availableDigits = x.split(".")[1]; // 10 000
  let remainingPI = availableDigits.length - position; //99 072
  //let removeRemainder = Math.round((availableDigits.length - position) / groupsOf) * groupsOf;
  let scrollDigits = availableDigits.substr(
    position,
    remainingPI - (remainingPI % groupsOf)
  );
  //let fixLogic = scrollDigits.length
  if (autoStart) {
    $("#pi").html(groupDigits(scrollDigits, true));
    startScrolling();
  } else {
    $("#pi").html(groupDigits(scrollDigits, true));
    scrollingPaused = !autoStart;
  }
}

function startScrolling() {
  let scroll = $("#pi").scrollTop();
  piScroller = setInterval(function() {
    scroll = parseInt(scroll) + 1;
    $("#pi")
      .addClass("scrollFix")
      .scrollTop(scroll)
      .removeClass("scrollFix"); //en hier :)
  }, 50);
  scrollingPi = true;
}

function pi(digit) {
  let pi = $("#pi").html();
  if (pi.split(/g*/)[4] == ".") pi = "3.14";
  if (digit === x.substr(p, 1)) {
    //Digit entered is correct.
    $("#pi").html(groupDigits(pi + digit, false));
    if (!struck) {
      streak++;
      hiScore(streak);
    } else if (!strikeOutReset) {
      $("#scoreHead").html("CORRECT");
      $("#levelHead").html("HINTS");
      $("#recordHead").html("WRONG");
      streak++;
      hiScore(streak);
    }
    p++;
    if (enableTimer) {
      if (!timerStarted) startTimer();
    }
  } else {
    //Digit is wrong.
    strike++;
    if (errorHint) {
      showHint(showHints);
    }
    if (strikeOutReset) {
      struck = true;
      navigator.vibrate(250);
      $("#strike").html(strike);
      $("#Inconsistencies").html(inconsistencyText);
      $("#mode").addClass("offline", 1000);
      $(".live")
        .removeClass("live")
        .addClass("dead", 200);
      if (!enableZen) {
        if (strike == strikeReset - 1) {
          $("#message").html(
            "Take a hint to avoid striking out.<br />Using hints disables scoring and allows you to explore Pi."
          );
        }
        if (strike > strikeReset - 1) {
          reset();
          navigator.vibrate([100, 100, 100, 100, 100]);
        }
      } else {
        showHint(1);
      }
    } else {
      navigator.vibrate(250);
      struck = true;
      $("#strike").html(strike);
      $("#Inconsistencies").html(inconsistencyText);
      $("#mode").addClass("offline", 1000);
      $(".live")
        .removeClass("live")
        .addClass("dead", 200);
      $("#scoreHead").html("CORRECT");
      $("#levelHead").html("HINTS");
      if (piMaster) {
        $("#level").html("0");
      }
      $("#recordHead").html("WRONG");
      $("#rec").html(strike);
    }
    if (enableTimer) {
      resetTimer(false);
    }
  }
  $("#pi")
    .addClass("scrollFix")
    .scrollTop($("#pi").height())
    .removeClass("scrollFix");
}

function piPos(digit) {
  let pos = $("#pi").html();
  if (pos == "Enter position...") {
    enteredPosition = parseInt(digit);
    pos =
      "Position: " +
      enteredPosition +
      "<br /><span style='font-size: large;'>(Tap to start scrolling)</span>";
  } else {
    if (parseInt((enteredPosition += digit)) <= x.split(".")[1].length - 100) {
      pos =
        "Position: " +
        enteredPosition +
        "<br /><span style='font-size: large;'>(Tap to start scrolling)</span>";
    } else {
      pos =
        x.split(".")[1].length +
        " is Ample PI." +
        "<br /><span style='font-size: large;'>(Valid range between 1 and " +
        (x.split(".")[1].length - 100) +
        ")</span>";
      enteredPosition = "";
    }
  }
  $("#pi").html(pos);
}

function searchPi(digit) {
  let pos = $("#pi").html();
  if (pos == "Search Pi...") {
    searchValue = digit;
  } else {
    searchValue += digit;
  }
  pos =
    "Find: " +
    searchValue +
    "<br /><span style='font-size: large;'>(Tap to find sequence position)</span>";
  $("#pi").html(pos);
  searchReset = false;
}

function resetSearch() {
  $("#pi").html("Search Pi...");
  searchValue = 0;
  searchingNextPi = false;
  searchCurrentPosition = 0;
  searchDisplayCurrentPi = 0;
  searchReset = true;
}

function groupDigits(digits, scroll) {
  let str = digits.toString().split(".");
  let re = new RegExp("(\\d)(?=(\\d{" + groupDigitsEvery + "})+$)", "g");
  re.global = true;
  if (str[0].length >= 4) {
    str[0] = str[0].replace(re, "$1 ");
  }
  re = new RegExp("(\\d{" + groupDigitsEvery + "})", "g");
  re.global = true;
  if (str[1] && str[1].length >= 4) {
    str[1] = str[1].replace(re, "$1 ");
  }
  if (scroll) {
    if (!scrollingAutoStart) {
      return (
        "<span style='font-size: large;'>Tap to start scrolling...</span><br/>" +
        str.join(".")
      );
    }
  }
  return str.join(".");
}

function hiScore(score) {
  if (score !== 0) {
    if (!struck) {
      let highScore = +localStorage["highScore"];
      let newLevel = +localStorage["level"];
      if (+score > highScore) {
        localStorage["highScore"] = score;
        if (score % 10 === 0) {
          newLevel++;
          localStorage["level"] = newLevel;
        }
        $("#level").html(localStorage["level"]);
        $("#rec").html(localStorage["highScore"]);
      }
    }
  }
  $("#score").html(score);
  $("#pi")
    .addClass("scrollFix")
    .scrollTop($("#pi").scrollTop())
    .removeClass("scrollFix");
}

function showHint(digits) {
  $("#hint, #xhint, .key").off("click");
  if (!strikeOutReset) {
    navigator.vibrate(250);
    $("#levelHead").html("HINTS");
    if (piMaster) {
      $("#level").html(0);
      piMaster = !piMaster;
    }
  }
  if (!struck) {
    navigator.vibrate(250);
    struck = !struck;
  } else {
    navigator.vibrate(10);
  }
  if (strikeOutReset) {
    $("#mode").addClass("offline", 1000);
    $(".live")
      .removeClass("live")
      .addClass("dead", 200);
  } else {
    $("#mode").addClass("offline", 1000);
    $(".live")
      .removeClass("live")
      .addClass("dead", 200);
    $("#scoreHead").html("CORRECT");
    $("#levelHead").html("HINTS");
    $("#recordHead").html("WRONG");
    $("#rec").html(strike);
    $("#score").html(streak);
  }
  resetTimer(false);
  $("#message").empty();
  let arrHint = x.substr(p, digits).split("");
  let i = 0;
  let hinterVal = setInterval(function() {
    !strikeOutReset
      ? $("#level").html(parseInt($("#level").html()) + 1)
      : false;
    $("#" + arrHint[i]).addClass("hintKey", 400);
    $("#" + arrHint[i]).removeClass("hintKey", 200);
    $("#0" + arrHint[i]).addClass("hintKey", 400);
    $("#0" + arrHint[i]).removeClass("hintKey", 200);
    i++;
    if (i == arrHint.length) {
      clearInterval(hinterVal);
      bindKeypad();
    }
  }, 600);
}

function bindKeypad() {
  $(".key").on("click", function(e) {
    let digit = $(this).attr("data-value");
    e.preventDefault();
    navigator.vibrate(10);
    if (scrollingPi || scrollingPaused) {
      reset();
    }
    if (enterPosition) {
      piPos(digit);
    } else if (searchingPi) {
      searchPi(digit);
      if (searchDisplayCurrentPi > 0) {
        searchDisplayCurrentPi = 0;
        searchingNextPi = false;
      }
    } else {
      pi(digit);
    }
  });
  $("#hint, #xhint").on("click", function() {
    showHint(showHints);
  });
}

function loadCheckPoint() {
  reset();
  streak = parseInt(level + "0");
  p = streak + 4;
  $("#score").html(streak);
  $("#pi").html(groupDigits(x.substr(0, p), false));
  $("#pi")
    .addClass("scrollFix")
    .scrollTop(1000000)
    .removeClass("scrollFix");
}

function showLoader(text) {
  $.mobile.loading("show", {
    text: text,
    textVisible: true,
    theme: "a",
    html: ""
  });
}

function hideLoader() {
  $.mobile.loading("hide");
}

function getTimeRemaining(endtime) {
  let t = Date.parse(endtime) - Date.parse(new Date());
  let seconds = Math.floor((t / 1000) % 60);
  let minutes = Math.floor((t / 1000 / 60) % 60);
  let hours = Math.floor((t / (1000 * 60 * 60)) % 24);
  let days = Math.floor(t / (1000 * 60 * 60 * 24));
  return {
    total: t,
    days: days,
    hours: hours,
    minutes: minutes,
    seconds: seconds
  };
}

function initializeClock(id, endtime) {
  let clock = document.getElementById(id);
  let daysSpan = clock.querySelector(".days");
  let hoursSpan = clock.querySelector(".hours");
  let minutesSpan = clock.querySelector(".minutes");
  let secondsSpan = clock.querySelector(".seconds");
  let clockHead = $("#clockHead");

  $("#clockHead").html(
    `<span style="font-size:large;">Counting down to:</span><br /> <span style="font-size:x-large;">Pi day ${endtime.getFullYear()}</span>`
  );

  function updateClock() {
    let t = getTimeRemaining(endtime);
    daysSpan.innerHTML = t.days;
    hoursSpan.innerHTML = ("0" + t.hours).slice(-2);
    minutesSpan.innerHTML = ("0" + t.minutes).slice(-2);
    secondsSpan.innerHTML = ("0" + t.seconds).slice(-2);

    if (t.total <= 0) {
      clearInterval(timeinterval);
      clockHead.html("Happy Pi day!");
      clockHead.addClass("hapPiDay");
      $("#secretMessage").addClass("showSecret");
      $(".ui-page-active").addClass("tgiPiDay");
      daysSpan.innerHTML = "00";
      hoursSpan.innerHTML = "00";
      minutesSpan.innerHTML = "00";
      secondsSpan.innerHTML = "00";
    }
    if (t.days < -1) {
      $("#secretMessage").removeClass("showSecret");
      $(".ui-page-active").removeClass("tgiPiDay");
      clockHead.html("Reset the counter...");
      $("#resetPiCountdownTimer").show();
    }
  }

  updateClock();
  var timeinterval = setInterval(updateClock, 1000);
}

function reset() {
  if (localStorage["highScore"] == null) {
    localStorage.setItem("highScore", "0");
  }
  if (localStorage["level"] == null) {
    localStorage.setItem("level", "0");
  }
  enableTimer
    ? $("#pi").html(`3.14...<br /><span style='font-size: large;'>
    (Timer starts on first digit.)</span>`)
    : $("#pi").html("3.14...");

  $("#scoreHead").html("STREAK");
  $("#levelHead").html("LEVEL");
  $("#recordHead").html("RECORD");
  $("#level").html(localStorage["level"]);
  level = localStorage["level"];
  $("#rec").html(localStorage["highScore"]);
  enableZen ? true : $("#levelBlock").removeClass("zen");
  p = 4;
  strike = 0;
  streak = 0;
  struck = false;
  if (typeof piScroller != "undefined") {
    clearInterval(piScroller);
    delete piScroller;
  }
  piMaster = true;
  scrollingPaused = false;
  scrollingPi = false;
  enterPosition = false;
  enteredPosition = 0;
  resetTimer(true);
  timerStarted = false;
  searchingPi = false;
  searchingNextPi = false;
  searchDisplayCurrentPi = 0;
  searchCurrentPosition = 0;
  searchValue = "";
  searchOccurrences = 0;
  searchReset = false;
  $("#score").html(streak);
  $("#streak").html(streak);
  $("#strike").empty();
  $("#Inconsistencies").empty();
  $("#mode").removeClass("offline", 1000);
  $(".dead")
    .removeClass("dead")
    .addClass("live", 200);
  $("#message").empty();
  $("#left-panel").panel("close");
}

// $(document).ready(function () {
//     main();
// });

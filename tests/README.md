# Testy Calm Commanderu

Automatické testy, které spouštějí **skutečný Z80 kód** CC a pluginů v emulátoru
a kontrolují výsledek. Vzorem je projekt Nextheus. Na CSpect ani SD image se
nesahá.

## Spuštění

Jednorázově:

```
pip install z80
```

Pak z kořene projektu:

```
test.bat              všechny testy (asi 15 s)
test.bat -v           s názvem každého testu
test.bat -k sort      jen testy, které mají v názvu "sort"
```

Testy si samy přeloží `cc.asm`, `dot/ccdot.asm` a pluginy stejným
`sjasmplus.exe` jako `compile.bat` (z kořene projektu). Symboly a listingy jdou
do `build/test/`. Binárky (`cc.bin`, `plugin/*.ccp`, `build/dot/ccd.bin` …)
vzniknou stejné jako z `compile.bat`.

Výsledek `OK (expected failures=5)` je v pořádku – viz *Známé chyby* níže.

## Co se testuje

| Soubor | Co |
|---|---|
| `test_core.py` | rutiny jádra CC v BASIC **i** dot sestavení: hledání podle masky (`wildcard_match_ci`, 1500 náhodných masek proti Pythonu), výběr pluginu podle přípony a velikosti, řazení panelu (jméno / přípona / datum, adresáře napřed, víc LFN stránek, katalog a LFN zůstávají spárované, MMU6 = 0 – regrese KS3), `NUM`, `D32B`, `showdate`, `showtime` |
| `test_dot_loader.py` | zavaděč `.cc`: obnova paměti a Next registrů po Quit, rezervace stránek, žádný zápis do $7FFD po M_P3DOS, chyby, spouštění NEX, vkládání příkazu do BASICu (řádek i program, skrytá čísla) |
| `test_dot_cc.py` | dot část CC: příkazy pro BAS/TAP (všechny stroje)/snapshoty, dlouhá jména, `dot_return`, `dot_entry` |
| `test_plugins.py` | ZIP, TRD, SCL a TAP: rozbalení/export se porovná s Pythonem (`zipfile`, vlastní rozbor TRD/SCL/TAP), +3DOS hlavičky, seek za 64K |
| `test_viewers.py` | text (řádky, CR/LF, zalomení, scrollování, hex/dec, hledání), BAS (výpis, hlavička +3DOS, posun o řádek = celé překreslení), ZX screen a NXI (obsah Layer 2 = převod v Pythonu, paleta, obnova registrů) |
| `test_music.py` | PT3, PT3 TurboSound, PT2, STC, STP, SQT: hraje s přerušením, MMU2 a IY při každém přerušení, ENTER = konec, SPACE = další |
| `test_edit.py` | editor: psaní, mazání, šipky, hex přepis, uložit, uložit jako, dotaz při odchodu – uložený soubor proti modelu v Pythonu |
| `test_files.py` | dir_info (počty a 32bitové velikosti, limit hloubky, chyby) a syscopy (kopie, přesun, mazání, přepis s dotazem, CAPS+SPACE, vnořený cíl) nad falešným esxDOS |
| `test_bookmarks.py` | záložky: přidání, limit 200, výběr, filtr, převod starého formátu |
| `test_settings.py` | nastavení: schémata, zachycení klávesy, konflikt, uložit/zrušit, zápis palety do registrů |

## Pomocné moduly

- `cctest.py` – překlad, emulovaný Next (stránky, MMU, Next registry, porty,
  klávesnice, přerušení), Z80N instrukce, `Typist` (mačkání kláves přes porty).
- `pluginhost.py` – falešný hostitel pro prohlížeče (`*.ccp` se `SERVICE_*`).
- `fakeesx.py` – falešný esxDOS (`RST $08`) se stromem souborů v Pythonu
  (chová se jako FAT: krátká i dlouhá jména, díry po smazaných položkách) a
  hostitel pro dir_info / syscopy / bookmarks / settings.
- `golden/*.json` – uložené obrazovky a seznamy souborů. Když se výstup
  **záměrně** změní: `set CC_UPDATE_GOLDEN=1` a `test.bat`, pak změny v
  `golden/` zkontrolovat v gitu.
- `fixtures/` – vzorky pro testy (PT2, STC, STP, starý `bookmark.cfg`).

## Známé chyby (expectedFailure)

Tyto testy popisují chování, které je dnes špatně. Dokud chyba trvá, hlásí se
jako *expected failure*. Až se opraví, ohlásí se jako *unexpected success* –
pak stačí smazat `@unittest.expectedFailure`.

1. **syscopy – mazání se zasekne, když v podadresáři něco nejde smazat**
   (`test_delete_error_in_subdir_is_reported`). V `delete_dir` po
   `call delete_dir` následuje `pop af`, který přepíše carry potomka; rodič
   adresář znovu otevře a mazání zkouší dokola. Totéž u stromu hlubšího než
   11 úrovní (`test_deep_tree_delete_reports_error`). `copy_dir` to řeší přes
   `childCarry`.
2. **syscopy – kopie stromu hlubšího než 11 úrovní hlásí úspěch, ale soubory
   od 12. úrovně chybí** (`test_deep_tree_copy_is_complete_or_fails`).
3. **dir_info – chyba v podadresáři se ztratí** (stejné `pop af`), výsledkem
   je neúplný součet hlášený jako úspěch (`test_unreadable_subdir_is_reported`).
4. **bookmarks – převod starého formátu vždy selže** „Cannot access
   c:/sys/bookmark.cfg.“ (`test_old_format_is_migrated`): po `F_READ`/`F_WRITE`
   se nenulové BC bere jako chyba, ale BC je počet přenesených bajtů.

## Nový test

Nejjednodušší je zkopírovat podobný test. Plugin prohlížeče:
`PluginHost(self, "jmeno_pluginu", "soubor", data, VIEWTYPE, keys=[...],
menu_sites=["plugin_start.input"])` (návěští, na kterém stojí `call call_input`
nabídky – klávesy dostane jen ono), pak `host.run()` a kontrola
`host.files`, `host.text()` nebo `host.snapshots`. Rutina jádra:
`m, s = cc_machine("basic")` a `m.call(s["rutina"], sp=STACK, hl=...)`.

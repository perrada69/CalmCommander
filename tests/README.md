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

## Co se testuje

| Soubor | Co |
|---|---|
| `test_core.py` | rutiny jádra CC v BASIC **i** dot sestavení: hledání podle masky (`wildcard_match_ci`, 1500 náhodných masek proti Pythonu), výběr pluginu podle přípony a velikosti, řazení panelu (jméno / přípona / datum, adresáře napřed, víc LFN stránek, katalog a LFN zůstávají spárované, MMU6 = 0 – regrese KS3), `NUM`, `D32B`, `showdate`, `showtime`, mapa paměti (všechny pevné stránky pod 96, nic se nepřekrývá, zavaděč rezervuje přesně je) |
| `test_dot_loader.py` | zavaděč `.cc`: obnova paměti a Next registrů po Quit, rezervace stránek, běh na 1MB Nextu, žádný zápis do $7FFD po M_P3DOS, chyby, spouštění NEX, vkládání příkazu do BASICu (řádek i program, skrytá čísla) |
| `test_dot_cc.py` | dot část CC: příkazy pro BAS/TAP (všechny stroje)/snapshoty, dlouhá jména, `dot_return`, `dot_entry` |
| `test_plugins.py` | ZIP, TRD, SCL a TAP: rozbalení/export se porovná s Pythonem (`zipfile`, vlastní rozbor TRD/SCL/TAP), +3DOS hlavičky, seek za 64K |
| `test_viewers.py` | text (řádky, CR/LF, zalomení, scrollování, hex/dec, hledání), BAS (výpis, hlavička +3DOS, posun o řádek = celé překreslení), ZX screen a NXI (obsah Layer 2 = převod v Pythonu, paleta, obnova registrů) |
| `test_music.py` | PT3, PT3 TurboSound, PT2, STC, STP, SQT: hraje s přerušením, MMU2 a IY při každém přerušení, ENTER = konec, SPACE = další |
| `test_edit.py` | editor: psaní, mazání, šipky, hex přepis, uložit, uložit jako, dotaz při odchodu – uložený soubor proti modelu v Pythonu |
| `test_files.py` | dir_info (počty a 32bitové velikosti, limit hloubky, chyby) a syscopy (kopie, přesun, mazání, přepis s dotazem, CAPS+SPACE, vnořený cíl) nad falešným esxDOS |
| `test_bookmarks.py` | záložky: přidání, limit 200, výběr, filtr, převod starého formátu |
| `test_config.py` | `cc.cfg`: soubory ze všech verzí (0.6–1.3, 1.4, 1.5+), převod z 1.4, rozbité klávesy (už jednou špatně převedený soubor, 1.4 spuštěná po 1.5) → výchozí barvy a klávesy, přepínače mimo rozsah, nový soubor |
| `test_settings.py` | nastavení: schémata, zachycení klávesy, konflikt, uložit/zrušit, zápis palety do registrů |
| `test_mouse.py` | kmouse v obou sestaveních: klik na nápovědu klávesy (skutečné texty z CC a pluginů), pravé tlačítko = BREAK, kolečko, pravidla v panelech / menu / dialogu / pluginu, `INKEY`, `KEYSCAN_UI`, dialogy potvrzení a přepsání souboru |

## Pomocné moduly

- `cctest.py` – překlad, emulovaný Next (stránky, MMU, Next registry, porty,
  klávesnice, kmouse, přerušení). Stroj je **1MB Next**: namapování stránky
  nad 95 nebo Layer 2 mimo první RAM čip shodí test (zavaděč `.cc` testuje
  i 2MB přes `DotEnv(total=224)`), Z80N instrukce, `Typist` (mačkání kláves
  přes porty), `keyscan()` (jako KEYSCAN v CC).
- `pluginhost.py` – falešný hostitel pro prohlížeče (`*.ccp` se `SERVICE_*`).
- `fakeesx.py` – falešný esxDOS (`RST $08`) se stromem souborů v Pythonu
  (chová se jako FAT: krátká i dlouhá jména, díry po smazaných položkách) a
  hostitel pro dir_info / syscopy / bookmarks / settings.
- `golden/*.json` – uložené obrazovky a seznamy souborů. Když se výstup
  **záměrně** změní: `set CC_UPDATE_GOLDEN=1` a `test.bat`, pak změny v
  `golden/` zkontrolovat v gitu.
- `fixtures/` – vzorky pro testy (PT2, STC, STP, starý `bookmark.cfg`).

## Opravené chyby, které testy našly

Testy, které je hlídají, aby se nevrátily:

1. **syscopy – mazání se zaseklo, když v podadresáři něco nešlo smazat**
   nebo byl strom hlubší než 11 úrovní (`test_delete_error_in_subdir_is_reported`,
   `test_deep_tree_delete_reports_error`). `pop af` po `call delete_dir`
   přepsal carry potomka a rodič mazal tentýž adresář dokola. Stejná oprava
   je i v `count_dir`.
2. **syscopy – kopie stromu hlubšího než 11 úrovní hlásila úspěch, ale
   soubory od 12. úrovně chyběly** (`test_deep_tree_copy_fails_instead_of_skipping`).
   Teď skončí chybou $7F a přesun pak zdroj nesmaže (`test_deep_tree_move_keeps_source`).
3. **dir_info – chyba v podadresáři se ztratila** (stejné `pop af`) a neúplný
   součet se hlásil jako úspěch (`test_unreadable_subdir_is_reported`).
4. **bookmarks – převod starého formátu vždy selhal** „Cannot access
   c:/sys/bookmark.cfg.“ (`test_old_format_is_migrated`): po `F_READ`/`F_WRITE`
   se nenulové BC bralo jako chyba, ale BC je počet přenesených bajtů.
5. **CC na 1MB Nextu psal do neexistující paměti** – data prohlížeče
   (stránka 97), pracovní stránka syscopy/dir_info (99, dot commandy 99-100) a
   Layer 2 obrázků NXI/SCR (98-103). Všechno je teď pod 96 (mapa v `cc.asm`);
   hlídá to každý test a `MemoryMap`.
6. **LFN pravého panelu přetékala do katalogu** od 421. položky (stránky
   60-79 zasahovaly do 74/76/78). Pravý panel má teď 44-63
   (`test_pages_fit_1mb_and_do_not_overlap`).
7. **Starý `cc.cfg` rozbil ovládání** – soubor z CC 0.6–1.3 (539–542 B) se
   převáděl jako formát 1.4, takže z výchozích barev a kláves vznikl nesmysl
   (šipky, ENTER, `5` = Quit…) a uložil se s verzí 1. Formát se teď pozná podle
   toho, co soubor přepsal, a neplatné klávesy (0, BREAK, duplicita) vrátí
   výchozí barvy i klávesy – opraví se tak i už uložené rozbité soubory
   (`test_config.py`).

Když najdeš chybu, kterou zatím neopravuješ, zapiš ji jako test s
`@unittest.expectedFailure` – sada zůstane zelená, a až chybu opravíš, test se
ohlásí jako *unexpected success*.

## Nový test

Nejjednodušší je zkopírovat podobný test. Plugin prohlížeče:
`PluginHost(self, "jmeno_pluginu", "soubor", data, VIEWTYPE, keys=[...],
menu_sites=["plugin_start.input"])` (návěští, na kterém stojí `call call_input`
nabídky – klávesy dostane jen ono), pak `host.run()` a kontrola
`host.files`, `host.text()` nebo `host.snapshots`. Rutina jádra:
`m, s = cc_machine("basic")` a `m.call(s["rutina"], sp=STACK, hl=...)`.

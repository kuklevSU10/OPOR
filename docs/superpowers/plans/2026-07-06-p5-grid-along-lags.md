# П5 «Сетка строго вдоль лаг» (v3.5-tz-p5-lag-grid) — Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** После прогона OPOR на слое `сеткаvb` остаются только линии вдоль лаг; появляется счётчик «лаг N шт» в логе и OPORDUMP. Опоры/LENGTH/таблица не меняются.

**Architecture:** Поперечное семейство строится и обрезается прежним кодом (по нему снимаются узлы/границы опор), затем физически удаляется в конце `opor-grid-build`. Флаг `*opor-keep-perp-grid*` возвращает старое поведение. Счётчик лаг = число уникальных смещений (рядов) лаг-линий + рёбер поперёк направления лаг.

**Tech Stack:** AutoLISP (AutoCAD 2023 рус), статическая проверка `verify_opor.py` (Python), golden-master сверка в AutoCAD по логам (LOGFILEMODE=1).

**Спека:** `docs/superpowers/specs/2026-07-06-p5-grid-along-lags-design.md`

**ВАЖНО — особенности процесса:**
- Git-репозитория в проекте НЕТ: шаги «commit» отсутствуют, бэкап — облачная синхронизация папки.
- Классический TDD невозможен (код исполняется только внутри AutoCAD): роль тестов играют (а) `verify_opor.py` — баланс скобок + резолв всех `opor-*` вызовов, гонять после КАЖДОЙ правки; (б) golden-master прогоны в AutoCAD руками пользователя, сверка по логам (читать самому с диска, кодировка CP1251).
- Файлы `OPOR/*.lsp` содержат кириллицу — редактировать инструментом Edit, НЕ пересохранять другой кодировкой. После правок с новыми русскими строками — проверить фрагмент через чтение файла (не должно быть кракозябр).

---

### Task 1: Восстановить verify_opor.py и снять базовую линию

**Files:**
- Create: `<scratchpad>/verify_opor.py` (scratchpad сессии; путь см. в системном окружении)

- [ ] **Step 1: Записать скрипт**

```python
import re, sys, pathlib
d = pathlib.Path(r"C:\Users\user\Documents\Таран\0_0_ по работке\плагин дмитрию\OPOR")
files = sorted(d.glob("opor-*.lsp"))
defs = set(); calls = {}; bal_bad = []
def strip(code):
    code = re.sub(r'"(\\.|[^"\\])*"', '""', code, flags=re.S)
    code = re.sub(r';[^\n]*', '', code)
    return code
for f in files:
    code = strip(f.read_text(encoding="utf-8", errors="replace"))
    b = code.count("(") - code.count(")")
    if b != 0: bal_bad.append((f.name, b))
    for m in re.finditer(r"\(defun\s+([\w\-\*/><=!\?\+\.]+)", code): defs.add(m.group(1).lower())
    for m in re.finditer(r"[\('\s](opor-[\w\-\?]+)", code): calls.setdefault(m.group(1).lower(), set()).add(f.name)
missing = {c: fs for c, fs in calls.items() if c not in defs}
print("files:", len(files), "defuns:", len(defs))
print("balance:", "OK" if not bal_bad else bal_bad)
if missing:
    print("MISSING:")
    for c, fs in sorted(missing.items()): print("  ", c, "<-", ", ".join(sorted(fs)))
    sys.exit(1)
print("PASS")
```

- [ ] **Step 2: Запустить на нетронутом коде**

Run: `python <scratchpad>/verify_opor.py`
Expected: `balance: OK`, `PASS` (примерно 218+ defuns). Если FAIL на нетронутом коде — скрипт-ложь (например, `opor-` внутри строк/имён слоёв): чинить регэксп, НЕ код.

---

### Task 2: Флаг и версия в конфиге

**Files:**
- Modify: `OPOR/opor-config.lsp:3` (версия), `:85` (рядом с *opor-boundary-lag-length-mode*)

- [ ] **Step 1: Версия**

Было:
```lisp
(setq *opor-version* "3.4-tz-fasteners") ; +ТЗ: площадь минус проёмы, крепёж (каркас)
```
Стало:
```lisp
(setq *opor-version* "3.5-tz-p5-lag-grid") ; +ТЗ П5: сетка в чертеже только вдоль лаг
```

- [ ] **Step 2: Флаг (после строки с *opor-boundary-lag-length-mode*)**

```lisp
(setq *opor-keep-perp-grid* nil)                 ; T = рисовать поперечное семейство как VBA (A/B-сверка)
```

- [ ] **Step 3: verify_opor.py** → PASS

---

### Task 3: opor-unregister-created в core

**Files:**
- Modify: `OPOR/opor-core.lsp` (после `opor-register-created`, строки 36-41)

- [ ] **Step 1: Добавить хелпер сразу после defun opor-register-created**

```lisp
(defun opor-unregister-created (obj / objects)
  (setq objects (opor-session-get 'created-objects))
  (if objects
    (opor-session-set 'created-objects (vl-remove obj objects)))
  obj)
```

Примечание: в `created-objects` лежат те же lisp-значения VLA-объектов, что и в списках линий grid-build (trim регистрирует каждую созданную линию тем же объектом) — `vl-remove` удаляет по ним корректно.

- [ ] **Step 2: verify_opor.py** → PASS

---

### Task 4: Сетка — счётчик лаг и удаление поперечных

**Files:**
- Modify: `OPOR/opor-grid.lsp` — defun-строка `opor-grid-build` (:200), блок создания рёбер (:250-255), хвост функции (:282-284); новый defun перед `opor-grid-build`

- [ ] **Step 1: Новый defun счётчика рядов (вставить перед `opor-grid-build`)**

```lisp
;; ТЗ П5: лага = ряд; отрезки одного ряда (разрезанные проёмом) — одна лага.
;; Ключ ряда — смещение начала линии поперёк направления лаг, округлённое до 1 мм.
(defun opor-lag-row-count (lines base row-axis / off key seen)
  (setq seen '())
  (foreach line lines
    (setq off (opor-dot (opor-v- (opor-curve-start line) base) row-axis))
    (setq key (opor-round off))
    (if (not (member key seen)) (setq seen (cons key seen))))
  (length seen))
```

- [ ] **Step 2: Локальные переменные defun opor-grid-build**

В списке локальных после `lag-length grid` добавить ` edge-lines row-axis perp-lines`:
```lisp
(defun opor-grid-build (session / boundary holes base dir step-x step-y vec perp bbox diag half range-v range-p center-v center-p half-v half-p raw-v raw-p v-lines p-lines lag-axis lag-lines lag-axis-vector grid-length outer-segs hole-segs boundary-outer-length boundary-holes-length boundary-length lag-length grid edge-lines row-axis perp-lines)
```

- [ ] **Step 3: Захватить созданные рёбра**

Было:
```lisp
      ;; физическое создание рёбер-лаг на сеткаvb (как explode в b2_mains)
      (cond
        ((= *opor-boundary-lag-length-mode* "all")
          (opor-create-boundary-lag-lines (append outer-segs hole-segs)))
        ((= *opor-boundary-lag-length-mode* "holes")
          (opor-create-boundary-lag-lines hole-segs)))
```
Стало:
```lisp
      ;; физическое создание рёбер-лаг на сеткаvb (как explode в b2_mains)
      (setq edge-lines
        (cond
          ((= *opor-boundary-lag-length-mode* "all")
            (opor-create-boundary-lag-lines (append outer-segs hole-segs)))
          ((= *opor-boundary-lag-length-mode* "holes")
            (opor-create-boundary-lag-lines hole-segs))
          (t '())))
```

- [ ] **Step 4: Хвост opor-grid-build — счётчик и удаление**

Было (конец функции):
```lisp
      ;; b3_fin: gridleng = Round(gridleng / 1000, 0) — банковское
      (opor-session-set 'lag-length-m (opor-round-half-even (/ lag-length 1000.0)))
      grid)))
```
Стало:
```lisp
      ;; b3_fin: gridleng = Round(gridleng / 1000, 0) — банковское
      (opor-session-set 'lag-length-m (opor-round-half-even (/ lag-length 1000.0)))
      ;; ТЗ П5: счётчик лаг (рядов) — по лаг-линиям и физическим рёбрам
      (setq row-axis (if (= lag-axis "perp") vec perp))
      (opor-session-set 'lag-row-count
        (opor-lag-row-count (append lag-lines edge-lines) base row-axis))
      ;; ТЗ П5: в чертеже остаются только лаги — поперечное семейство
      ;; удаляется ПОСЛЕ снятия node-points/endpoint-points (они уже в grid)
      (setq perp-lines (if (= lag-axis "perp") v-lines p-lines))
      (if (not *opor-keep-perp-grid*)
        (progn
          (foreach line perp-lines
            (opor-delete-object line)
            (opor-unregister-created line))
          (opor-session-set 'perp-lines-removed (length perp-lines))
          (setq grid
            (subst
              (cons (if (= lag-axis "perp") 'v-lines 'p-lines) '())
              (assoc (if (= lag-axis "perp") 'v-lines 'p-lines) grid)
              grid))
          (opor-session-set 'grid grid))
        (opor-session-set 'perp-lines-removed 0))
      grid)))
```

- [ ] **Step 5: verify_opor.py** → PASS (в т.ч. резолв `opor-lag-row-count`, `opor-unregister-created`, `opor-round`)

---

### Task 5: Лог завершения и OPORDUMP

**Files:**
- Modify: `OPOR/opor-core.lsp` — хелпер после `opor-fasteners-log-text` (:123), лог Const (:150-153), лог Var (:212-214)
- Modify: `OPOR/opor-dump.lsp` — сессионный блок (перед `"\n    support split: vertices="`, :179)

- [ ] **Step 1: Хелпер в core (сразу после defun opor-fasteners-log-text)**

```lisp
;; ТЗ П5: счётчик лаг (рядов) для лога завершения
(defun opor-lag-count-log-text (/ n)
  (setq n (opor-session-get 'lag-row-count))
  (if (numberp n)
    (strcat ", лаг=" (itoa n) " шт")
    ""))
```

- [ ] **Step 2: Лог Const**

Было:
```lisp
                      ", длина лаг="
                      (itoa (opor-session-get 'lag-length-m))
                      " м"
                      (opor-fasteners-log-text)
```
Стало:
```lisp
                      ", длина лаг="
                      (itoa (opor-session-get 'lag-length-m))
                      " м"
                      (opor-lag-count-log-text)
                      (opor-fasteners-log-text)
```

- [ ] **Step 3: Лог Var**

Было:
```lisp
                          ", длина лаг="
                          (itoa (opor-session-get 'lag-length-m))
                          " м, ошибок высот="
```
Стало:
```lisp
                          ", длина лаг="
                          (itoa (opor-session-get 'lag-length-m))
                          " м"
                          (opor-lag-count-log-text)
                          ", ошибок высот="
```

- [ ] **Step 4: OPORDUMP — в сессионный блок**

В `opor-dump.lsp` внутри большого strcat сессионного блока вставить ПЕРЕД строкой `"\n    support split: vertices="`:
```lisp
        "\n    П5 лаги: рядов="
        (itoa (opor-dump-session-int 'lag-row-count))
        ", поперечных удалено="
        (itoa (opor-dump-session-int 'perp-lines-removed))
```

- [ ] **Step 5: verify_opor.py** → PASS

- [ ] **Step 6: Проверка кириллицы после правок**

Run: `iconv -f utf-8 -t utf-8 OPOR/opor-core.lsp >/dev/null && echo utf8-ok` (или прочитать фрагменты с новыми строками)
Expected: новые строки «лаг= шт», «П5 лаги: рядов=» читаются без кракозябр в той же кодировке, что и остальной файл.

---

### Task 6: Golden-master S1 в AutoCAD (пользователь запускает, лог читаю сам)

**Процедура (пользователю):** открыть `эталоны/S1_lisp.dwg` → `OPORCLEAN` → `All` → `APPLOAD` свежего `OPOR/opor-loader.lsp` → `OPOR` → `Const`, ввод стандартный S1 (контур кликом, нач.точка `0,0`, направление `@1<0`, таблица `7000,3000`, остальное Enter) → `DUMPALL` → `OPORDUMP` → сказать «готово».

- [ ] **Step 1: Дождаться «готово», прочитать свежий лог** (`ls -lat *.log`, iconv CP1251)

- [ ] **Step 2: Сверить**

| Показатель | Ожидание |
|---|---|
| Версия в логе загрузки | 3.5-tz-p5-lag-grid |
| Опоры | 60, все цвет 221 |
| Таблица | бит-в-бит со старым эталоном (1=60, ITOG=60, AREA=15, LENGTH=30, VECTOR/PERP=600, QC/QR=0) |
| сеткаvb (DUMPALL) | **10 линий / 30000 мм** (было 16/60000) |
| OPORDUMP grid-v | 9 линий, 27.00 м |
| OPORDUMP grid-p | **0 линий, 0.00 м** |
| OPORDUMP grid-edge | 1 линия (3000 мм) |
| OPORDUMP «П5 лаги» | рядов=10, поперечных удалено=6 |
| Финальный лог | «…длина лаг=30 м, лаг=10 шт.» |
| Инвариант | длина линий сеткаvb = LENGTH×1000 = 30000 |

Если сошлось → Task 7. Если нет → systematic-debugging, правка, повторный прогон.

---

### Task 7: Golden-master S3 (проём) и S2 (Var)

- [ ] **Step 1: S3** — `эталоны/S3_lisp.dwg`, тот же цикл (проём уже на областиvb).

Ожидание: опоры 68; таблица бит-в-бит со старым эталоном (AREA=14 после П9-площади); grid-p = 0; grid-edge = 3; сумма длин сеткаvb = LENGTH×1000; «П5 лаги: рядов=N» — снять фактическое N (для эталона).

- [ ] **Step 2: S2 (Var)** — `эталоны/s2_lisp.dwg`, режим Var, стандартный ввод S2 (пол 101, доска 27, лага 49).

Ожидание: опоры 68, раскладка цвет1×1/цвет2×24/цвет221×43 бит-в-бит; таблица поле в поле (CHPOL=101, AREA=14); grid-p = 0; сумма сеткаvb = LENGTH×1000; в логе Var «…м, лаг=N шт, ошибок высот=0».

- [ ] **Step 3 (опционально, если что-то мутное): флаг отката**

Пользователь: `(setq *opor-keep-perp-grid* T)` в командной строке → повторный прогон S1 → должны вернуться старые числа (16/60000). Потом `(setq *opor-keep-perp-grid* nil)`.

---

### Task 8: Обновить эталоны и документацию

**Files:**
- Modify: `эталоны/README.md` — числа слоя сеткаvb для S1/S2/S3/S5 (новые: только лаги; S1 = 10/30000), пометка «после П5 v3.5», инвариант «сеткаvb = LENGTH×1000», фактические «рядов=N» из прогонов
- Modify: `HANDOFF.md` — п.5 статус ✅ СДЕЛАН (v3.5): что изменилось, флаг *opor-keep-perp-grid*, счётчик лаг; версия порта в шапке
- Modify: `OPOR/IMPLEMENTATION_STATUS.md` — строка о v3.5
- Modify: память `opor-plugin-project.md` — абзац этапа ТЗ: П5 ✅ (v3.5), кратко

- [ ] **Step 1: Обновить эталоны/README.md** (числа из фактических прогонов Task 6-7)
- [ ] **Step 2: Обновить HANDOFF.md + IMPLEMENTATION_STATUS.md**
- [ ] **Step 3: Обновить память проекта**
- [ ] **Step 4: Перенести свежие логи в logs/** (`mv -f *.log logs/`)

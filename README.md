# Smart Notes

Smart Notes - учебный заметочный веб-сервис с REST API на языке Haskell  
Возможно, если не найдётся хорошей NLP библиотеки, то фишки с анализом текста будут вынесены в микросервис на Python на FastAPI

## Планируемые Endpoints

- `api/notes/` - api заметок
  - `GET api/notes/all` - список всех заметок: заголовки, id
    - `sort_by` - сортировать по [`'date'`/`'alpha'`, опционально]
  - `GET api/notes/by_tag` - список заметок с указанным тегом: заголовки, id
    - `tag` - имя тега [существующие имена тегов/`'untagged'`, `String`]
    - `top` - количество [`Int` положительное, опционально], если не указано - все
    - `sort_by` - сортировать по [`'date'`/`'alpha'`, опционально]
  - `GET api/notes/by_id` - получить заметку по id: заголовок, текст, теги
    - `id` - id заметки [`Int`]
  - `POST api/notes` - добавить новую заметку
    - `header` - заголовок [`String`]
    - `text` - основной текст заметки [`String`]
    - `tags` - список тегов [`[String]`, опционально], если указан `'auto'` - список тегов формируется автоматически на основе статистического и семантического анализа сущестующих тегов
  - `DELETE api/notes` - удалить заметку 
    - `id` - id заметки [`Int`]
  - `UPDATE api/notes` - обновить заметку 
    - `id` - id заметки [`Int`]
    - `header` - заголовок [`String`, опционально]
    - `text` - основной текст заметки [`String`, опционально]
    - `tags` - список тегов [`[String]`, опционально], если указан `'auto'` - список тегов формируется автоматически на основе статистического и семантического анализа сущестующих тегов
    - `append_text` - не удалять старый текст [`Bool`, опционально, по-умолчанию - `False`]
    - `append_tags` - не удалять старыe теги [`Bool`, опционально, по-умолчанию - `False`]
  - `GET api/notes/find` - поиск по заметкам
    - `query` - запрос [`String`]
  

- `api/tags/` - api тегов
  - `GET api/tags/all` - список всех тегов
  - `POST api/tags` - создать тег
    - `tag` - имя тега [`String`]
    - `auto_associate` - найти соответствующие тегу заметки автоматически [`Bool`, опционально, по-умолчанию - `False`]
  - `DELETE api/tags` - удалить тег
    - `tag` - имя тега [существующее имя тега, `String`]
    - `keep_notes` - оставить заметки [`Bool`]
  - `GET api/tags/find` - поиск по тегам
    - `query` - запрос [`String`]

## Этапы реализации

- [ ] Проработка, прототипирование и согласование endpoints и функциональности
- [ ] Изучение фреймворков
- [ ] Реализация заметок, прикручивание базы данных
- [ ] Реализация простых тегов
- [ ] Реализация поиска (мб. расстояние Левенштейна)
- [ ] Реализация умных тегов
- [ ] Полное покрытие unit-тестами
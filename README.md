# Smart Notes

Smart Notes - учебный заметочный веб-сервис с возможностями анализа текста с REST API на языке Haskell 

## TO-DO
- [ ] Implement statistics
- [ ] Implement NLP features
- [ ] Divide and conquer the code in Main into files
- [ ] Clean up project files

## Особенности API
### Типы возвращаемых данных
```haskell
Tag: 
  tagId :: Int,
  tagName :: String

Note: 
  noteId :: Int,
  noteHeader :: String,
  noteTags :: [Int],
  noteText :: String

TextStats:
  symbols_ :: Int,
  words_ :: Int,
  sents_ :: Int

TextSplits:
  _words_ :: [String],
  _sents_ :: [String]
```

### Routing

- `/api/notes/` - api заметок
  - [x] `GET .` - список всех заметок $\longrightarrow$ `[Note]`
  - [x] `GET ./by_tag/:tag` - список заметок с указанным тегом $\longrightarrow$ `[Note]`
    - `tag :: Int` - id тега для поиска 
  - [x] `GET ./:id` - получить заметку по id $\longrightarrow$ заметка `Note`
    - `id :: Int` - id заметки
  - [x] `POST .` - добавить новую заметку $\longrightarrow$ обновлённый список :: `[Note]`
    - `header :: String` - заголовок
    - `text :: String` - основной текст заметки
    - `tags :: [Int]` - список тегов
  - [x] `DELETE ./:id` - удалить заметку по `id`: `Int` -> обновлённый список :: `[Note]`\
    - `id :: Int` - id заметки
  - [x] `GET ./search` - найти подходящие заметки с помощью нечёткого поиска по заголовкам и тексту заметок$\longrightarrow$ найденные заметки `[Note]`
    - `query: String` - запрос
  - [x] `GET ./stats/:id` - получить статистики о заметке $\longrightarrow$ `TextStats`
    - `id :: Int` - id заметки
  - [x] `GET ./splits/:id` - получить статистики о заметке $\longrightarrow$ `TextSplits`
    - `id :: Int` - id заметки 
  - [ ] `GET ./summary/:id` - получить автоматически сегенрированный реферат заметки
    - `id :: Int` - id заметки

- `api/tags/` - api тегов
  - [x] `GET .` - список всех тегов $\longrightarrow$ список всех тегов `[Tag]`
  - [ ] `POST ./new_match/:name` создать новый тег и автоматически найти и добавить в заметки по семантике $\longrightarrow$ обновлённый список тегов `[Tag]`
    - `name :: String` - имя нового тега
  - [x] `POST ./:name` - создать тег $\longrightarrow$ обновлённый список тегов `[Tag]`
    - `name :: String` - имя нового тега
  - [x] `DELETE ./:id` - удалить тег по id $\longrightarrow$ обновлённый список тегов `[Tag]`
    - `id :: Int` - id тега для удаления
  - [x] `DELETE ./by_name/:name` - удалить тег по имени $\longrightarrow$ обновлённый список тегов `[Tag]`
    - `name :: String` - имя нового тега
  - [x] `GET ./search` - найти подходящие теги с помощью нечёткого поиска по именам $\longrightarrow$ найденные теги `[Tag]`
    - `query :: String` - запрос
  - [x] `GET ./stats/:id` - статистика использования тега и заметок с ним $\longrightarrow$ `{uses_num: Int, uses_stats: TextStats}`
    - `id :: Int` - id тега
  - [x] `GET ./stats/by_name/:name` - статистика использования тега и заметок с ним $\longrightarrow$ `{uses_num: Int, uses_stats: TextStats}`
    - `name :: String` - имя тега
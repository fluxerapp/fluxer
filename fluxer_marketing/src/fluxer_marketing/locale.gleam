//// Copyright (C) 2026 Fluxer Contributors
////
//// This file is part of Fluxer.
////
//// Fluxer is free software: you can redistribute it and/or modify
//// it under the terms of the GNU Affero General Public License as published by
//// the Free Software Foundation, either version 3 of the License, or
//// (at your option) any later version.
////
//// Fluxer is distributed in the hope that it will be useful,
//// but WITHOUT ANY WARRANTY; without even the implied warranty of
//// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
//// GNU Affero General Public License for more details.
////
//// You should have received a copy of the GNU Affero General Public License
//// along with Fluxer. If not, see <https://www.gnu.org/licenses/>.

pub type Locale {
  Ar
  Bg
  Cs
  Da
  De
  El
  EnGB
  EnUS
  EsES
  Es419
  Fi
  Fr
  He
  Hi
  Hr
  Hu
  Id
  It
  Ja
  Ko
  Lt
  Nl
  No
  Pl
  PtBR
  Ro
  Ru
  SvSE
  Th
  Tr
  Uk
  Vi
  ZhCN
  ZhTW
}

pub fn get_code_from_locale(locale: Locale) -> String {
  case locale {
    Ar -> "ar"
    Bg -> "bg"
    Cs -> "cs"
    Da -> "da"
    De -> "de"
    El -> "el"
    EnGB -> "en-GB"
    EnUS -> "en-US"
    EsES -> "es-ES"
    Es419 -> "es-419"
    Fi -> "fi"
    Fr -> "fr"
    He -> "he"
    Hi -> "hi"
    Hr -> "hr"
    Hu -> "hu"
    Id -> "id"
    It -> "it"
    Ja -> "ja"
    Ko -> "ko"
    Lt -> "lt"
    Nl -> "nl"
    No -> "no"
    Pl -> "pl"
    PtBR -> "pt-BR"
    Ro -> "ro"
    Ru -> "ru"
    SvSE -> "sv-SE"
    Th -> "th"
    Tr -> "tr"
    Uk -> "uk"
    Vi -> "vi"
    ZhCN -> "zh-CN"
    ZhTW -> "zh-TW"
  }
}

pub fn get_locale_from_code(code: String) -> Result(Locale, Nil) {
  case code {
    "ar" -> Ok(Ar)
    "bg" -> Ok(Bg)
    "cs" -> Ok(Cs)
    "da" -> Ok(Da)
    "de" -> Ok(De)
    "el" -> Ok(El)
    "en-GB" | "en-gb" -> Ok(EnGB)
    "en-US" | "en-us" | "en" -> Ok(EnUS)
    "es-ES" | "es-es" -> Ok(EsES)
    "es-419" -> Ok(Es419)
    "fi" -> Ok(Fi)
    "fr" -> Ok(Fr)
    "he" -> Ok(He)
    "hi" -> Ok(Hi)
    "hr" -> Ok(Hr)
    "hu" -> Ok(Hu)
    "id" -> Ok(Id)
    "it" -> Ok(It)
    "ja" -> Ok(Ja)
    "ko" -> Ok(Ko)
    "lt" -> Ok(Lt)
    "nl" -> Ok(Nl)
    "no" -> Ok(No)
    "pl" -> Ok(Pl)
    "pt-BR" | "pt-br" -> Ok(PtBR)
    "ro" -> Ok(Ro)
    "ru" -> Ok(Ru)
    "sv-SE" | "sv-se" | "sv" -> Ok(SvSE)
    "th" -> Ok(Th)
    "tr" -> Ok(Tr)
    "uk" -> Ok(Uk)
    "vi" -> Ok(Vi)
    "zh-CN" | "zh-cn" -> Ok(ZhCN)
    "zh-TW" | "zh-tw" -> Ok(ZhTW)
    _ -> Error(Nil)
  }
}

pub fn get_locale_name(locale: Locale) -> String {
  case locale {
    Ar -> "العربية"
    Bg -> "Български"
    Cs -> "Čeština"
    Da -> "Dansk"
    De -> "Deutsch"
    El -> "Ελληνικά"
    EnGB -> "English"
    EnUS -> "English (US)"
    EsES -> "Español (España)"
    Es419 -> "Español (Latinoamérica)"
    Fi -> "Suomi"
    Fr -> "Français"
    He -> "עברית"
    Hi -> "हिन्दी"
    Hr -> "Hrvatski"
    Hu -> "Magyar"
    Id -> "Bahasa Indonesia"
    It -> "Italiano"
    Ja -> "日本語"
    Ko -> "한국어"
    Lt -> "Lietuvių"
    Nl -> "Nederlands"
    No -> "Norsk"
    Pl -> "Polski"
    PtBR -> "Português (Brasil)"
    Ro -> "Română"
    Ru -> "Русский"
    SvSE -> "Svenska"
    Th -> "ไทย"
    Tr -> "Türkçe"
    Uk -> "Українська"
    Vi -> "Tiếng Việt"
    ZhCN -> "简体中文"
    ZhTW -> "繁體中文"
  }
}

pub fn get_flag_code(locale: Locale) -> String {
  case locale {
    Ar -> "1f1f8-1f1e6"
    Bg -> "1f1e7-1f1ec"
    Cs -> "1f1e8-1f1ff"
    Da -> "1f1e9-1f1f0"
    De -> "1f1e9-1f1ea"
    El -> "1f1ec-1f1f7"
    EnGB -> "1f1ec-1f1e7"
    EnUS -> "1f1fa-1f1f8"
    EsES -> "1f1ea-1f1f8"
    Es419 -> "1f30e"
    Fi -> "1f1eb-1f1ee"
    Fr -> "1f1eb-1f1f7"
    He -> "1f1ee-1f1f1"
    Hi -> "1f1ee-1f1f3"
    Hr -> "1f1ed-1f1f7"
    Hu -> "1f1ed-1f1fa"
    Id -> "1f1ee-1f1e9"
    It -> "1f1ee-1f1f9"
    Ja -> "1f1ef-1f1f5"
    Ko -> "1f1f0-1f1f7"
    Lt -> "1f1f1-1f1f9"
    Nl -> "1f1f3-1f1f1"
    No -> "1f1f3-1f1f4"
    Pl -> "1f1f5-1f1f1"
    PtBR -> "1f1e7-1f1f7"
    Ro -> "1f1f7-1f1f4"
    Ru -> "1f1f7-1f1fa"
    SvSE -> "1f1f8-1f1ea"
    Th -> "1f1f9-1f1ed"
    Tr -> "1f1f9-1f1f7"
    Uk -> "1f1fa-1f1e6"
    Vi -> "1f1fb-1f1f3"
    ZhCN -> "1f1e8-1f1f3"
    ZhTW -> "1f1f9-1f1fc"
  }
}

pub fn all_locales() -> List(Locale) {
  [
    Ar,
    Bg,
    Cs,
    Da,
    De,
    El,
    EnGB,
    EnUS,
    EsES,
    Es419,
    Fi,
    Fr,
    He,
    Hi,
    Hr,
    Hu,
    Id,
    It,
    Ja,
    Ko,
    Lt,
    Nl,
    No,
    Pl,
    PtBR,
    Ro,
    Ru,
    SvSE,
    Th,
    Tr,
    Uk,
    Vi,
    ZhCN,
    ZhTW,
  ]
}

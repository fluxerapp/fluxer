/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {EmailTranslations} from '../types';

export const cs: EmailTranslations = {
	passwordReset: {
		subject: 'Obnovení hesla k účtu Fluxer',
		body: `Dobrý den, {username},

požádali jste o obnovení hesla k vašemu účtu Fluxer. Prosím, klikněte na odkaz níže a nastavte si nové heslo:

{resetUrl}

Pokud jste o obnovení hesla nežádali, můžete tento e-mail bezpečně ignorovat.

Tento odkaz vyprší za 1 hodinu.

– Tým Fluxer`,
	},
	emailVerification: {
		subject: 'Ověřte svou e-mailovou adresu pro Fluxer',
		body: `Dobrý den, {username},

prosíme, ověřte svoji e-mailovou adresu pro účet Fluxer kliknutím na odkaz níže:

{verifyUrl}

Pokud jste si účet Fluxer nevytvořili vy, můžete tento e-mail bezpečně ignorovat.

Tento odkaz vyprší za 24 hodin.

– Tým Fluxer`,
	},
	ipAuthorization: {
		subject: 'Povolte přihlášení z nové IP adresy',
		body: `Dobrý den, {username},

zaznamenali jsme pokus o přihlášení k vašemu účtu Fluxer z nové IP adresy:

IP adresa: {ipAddress}
Místo: {location}

Pokud jste to byli vy, prosím, povolte tuto IP adresu kliknutím na odkaz níže:

{authUrl}

Pokud jste se přihlásit nepokoušeli, ihned si prosím změňte heslo.

Tento autorizační odkaz vyprší za 30 minut.

– Tým Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'Váš účet Fluxer byl dočasně deaktivován',
		body: `Dobrý den, {username},

váš účet Fluxer byl dočasně deaktivován z důvodu podezřelé aktivity.

{reason, select,
	null {}
	other {Důvod: {reason}

}}Abyste znovu získali přístup ke svému účtu, musíte si obnovit heslo:

{forgotUrl}

Po obnovení hesla se budete moci znovu přihlásit.

Pokud se domníváte, že k tomuto kroku došlo omylem, obraťte se prosím na náš tým podpory.

– Bezpečnostní tým Fluxer`,
	},
	accountTempBanned: {
		subject: 'Váš účet Fluxer byl dočasně pozastaven',
		body: `Dobrý den, {username},

váš účet Fluxer byl dočasně pozastaven kvůli porušení našich Smluvních podmínek nebo Pravidel komunity.

Doba trvání: {durationHours, plural,
	=1 {1 hodina}
	other {# hodin}
}
Pozastaveno do: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Důvod: {reason}}
}

Během této doby nebudete mít ke svému účtu přístup.

Doporučujeme vám prostudovat naše:
- Smluvní podmínky: {termsUrl}
- Pravidla komunity: {guidelinesUrl}

Pokud se domníváte, že toto rozhodnutí o vynucení pravidel je nesprávné nebo neopodstatněné, můžete podat odvolání na adresu appeals@fluxer.app z této e-mailové adresy. Prosím, jasně vysvětlete, proč si myslíte, že rozhodnutí bylo chybné. Vaše odvolání přezkoumáme a odpovíme vám s naším závěrem.

– Bezpečnostní tým Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'Váš účet Fluxer je naplánován k odstranění',
		body: `Dobrý den, {username},

váš účet Fluxer byl naplánován k trvalému odstranění z důvodu porušení našich Smluvních podmínek nebo Pravidel komunity.

Plánované datum odstranění: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Důvod: {reason}}
}

Jedná se o závažné vynucovací opatření. Vaše data budou v plánovaném termínu trvale smazána.

Doporučujeme vám prostudovat naše:
- Smluvní podmínky: {termsUrl}
- Pravidla komunity: {guidelinesUrl}

PROCES ODVOlÁNÍ:
Pokud se domníváte, že toto rozhodnutí o vynucení pravidel je nesprávné nebo neopodstatněné, máte 30 dní na podání odvolání na adresu appeals@fluxer.app z této e-mailové adresy.

Ve svém odvolání prosím:
- Jasně vysvětlete, proč si myslíte, že rozhodnutí je nesprávné nebo neopodstatněné
- Uveďte všechny relevantní důkazy nebo kontext

Člen bezpečnostního týmu Fluxer vaše odvolání přezkoumá a může dočasně pozastavit plánované odstranění, dokud nebude vydáno konečné rozhodnutí.

– Bezpečnostní tým Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'Odstranění vašeho účtu Fluxer bylo naplánováno',
		body: `Dobrý den, {username},

mrzí nás, že odcházíte! Odstranění vašeho účtu Fluxer bylo naplánováno.

Plánované datum odstranění: {deletionDate, date, full} {deletionDate, time, short}

DŮLEŽITÉ: Odstranění můžete kdykoli před {deletionDate, date, full} {deletionDate, time, short} zrušit jednoduše tím, že se znovu přihlásíte ke svému účtu.

NEŽ ODEJDETE:
Panel ochrany soukromí v nastavení uživatele vám umožňuje:
- Smazat vaše zprávy na platformě
- Exportovat si důležitá data před odchodem

Vezměte prosím na vědomí: Jakmile bude váš účet odstraněn, nebude již možné vaše zprávy smazat. Pokud chcete své zprávy odstranit, proveďte to prosím přes Panel ochrany soukromí před definitivním smazáním účtu.

Pokud si to rozmyslíte, stačí se znovu přihlásit a odstranění zrušit.

– Tým Fluxer`,
	},
	inactivityWarning: {
		subject: 'Váš účet Fluxer bude odstraněn kvůli neaktivitě',
		body: `Dobrý den, {username},

všimli jsme si, že jste se ke svému účtu Fluxer nepřihlásili déle než 2 roky.

Poslední přihlášení: {lastActiveDate, date, full} {lastActiveDate, time, short}

V rámci naší politiky uchovávání dat jsou neaktivní účty automaticky naplánovány k odstranění. Váš účet bude trvale odstraněn dne:

Plánované datum odstranění: {deletionDate, date, full} {deletionDate, time, short}

JAK ZACHOVAT SVŮJ ÚČET:
Stačí se před datem odstranění přihlásit ke svému účtu na {loginUrl}. Není potřeba dělat nic dalšího.

CO SE STANE, POKUD SE NEPŘIHLÁSÍTE:
- Váš účet a všechna související data budou trvale odstraněna
- Vaše zprávy budou anonymizovány (přiřazeny uživateli „Smazaný uživatel“)
- Tento krok je nevratný

CHCETE SMAZAT SVÉ ZPRÁVY?
Pokud chcete své zprávy odstranit ještě před smazáním účtu, přihlaste se prosím a použijte Panel ochrany soukromí v nastavení uživatele.

Budeme rádi, pokud se na Fluxer vrátíte!

– Tým Fluxer`,
	},
	harvestCompleted: {
		subject: 'Váš export dat z Fluxer je připraven',
		body: `Dobrý den, {username},

váš export dat byl dokončen a je připraven ke stažení!

Souhrn exportu:
- Celkový počet zpráv: {totalMessages, number}
- Velikost souboru: {fileSizeMB} MB
- Formát: ZIP archiv se soubory JSON

Stáhnout data: {downloadUrl}

DŮLEŽITÉ: Tento odkaz ke stažení vyprší {expiresAt, date, full} {expiresAt, time, short}

Co je součástí exportu:
- Všechny vaše zprávy uspořádané podle kanálů
- Metadat­a kanálů
- Váš uživatelský profil a informace o účtu
- Členství v guildách a nastavení
- Relace přihlášení a bezpečnostní informace

Data jsou organizována ve formátu JSON pro snadné zpracování a analýzu.

Pokud máte k exportu dat jakékoli dotazy, kontaktujte prosím support@fluxer.app

– Tým Fluxer`,
	},
	unbanNotification: {
		subject: 'Pozastavení vašeho účtu Fluxer bylo zrušeno',
		body: `Dobrý den, {username},

dobrá zpráva! Pozastavení vašeho účtu Fluxer bylo zrušeno.

Důvod: {reason}

Nyní se můžete znovu přihlásit ke svému účtu a pokračovat v používání Fluxer.

– Bezpečnostní tým Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'Váš účet Fluxer je naplánován k odstranění',
		body: `Dobrý den, {username},

váš účet Fluxer byl naplánován k trvalému odstranění.

Plánované datum odstranění: {deletionDate, date, full} {deletionDate, time, short}
Důvod: {reason}

Jedná se o závažné vynucovací opatření. Data vašeho účtu budou v plánovaném termínu trvale odstraněna.

Pokud se domníváte, že toto rozhodnutí je nesprávné, můžete podat odvolání na adresu appeals@fluxer.app z této e-mailové adresy.

– Bezpečnostní tým Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'Váš darovaný Fluxer Premium byl zrušen',
		body: `Dobrý den, {username},

chtěli bychom vás informovat, že darovaný Fluxer Premium, který jste uplatnili, byl zrušen z důvodu platebního sporu (chargeback) zahájeného původním plátcem.

Prémiové výhody byly z vašeho účtu odebrány. K tomuto kroku došlo proto, že platba za dárek byla napadena a vrácena.

Pokud k tomu máte jakékoli dotazy, kontaktujte prosím support@fluxer.app.

– Tým Fluxer`,
	},
	reportResolved: {
		subject: 'Vaše nahlášení na Fluxer bylo posouzeno',
		body: `Dobrý den, {username},

vaše nahlášení (ID: {reportId}) bylo posouzeno naším Bezpečnostním týmem.

Odpověď Bezpečnostního týmu:
{publicComment}

Děkujeme, že pomáháte udržovat Fluxer bezpečným pro všechny. Všechna nahlášení bereme vážně a velmi si vážíme vašeho přínosu pro naši komunitu.

Pokud máte k tomuto rozhodnutí jakékoli dotazy nebo připomínky, kontaktujte prosím safety@fluxer.app.

– Bezpečnostní tým Fluxer`,
	},
	dsaReportVerification: {
		subject: 'Ověřte svůj e-mail pro nahlášení DSA',
		body: `Dobrý den,

použijte následující ověřovací kód k odeslání nahlášení podle Zákona o digitálních službách na Fluxer:

{code}

Tento kód vyprší {expiresAt, date, full} {expiresAt, time, short}.

Pokud jste o toto nepožádali, můžete tento e-mail ignorovat.

– Bezpečnostní tým Fluxer`,
	},
	registrationApproved: {
		subject: 'Vaše registrace na Fluxer byla schválena',
		body: `Dobrý den, {username},

skvělé zprávy! Vaše registrace na Fluxer byla schválena.

Nyní se můžete přihlásit do aplikace Fluxer na:
{channelsUrl}

Vítejte v komunitě Fluxer!

– Tým Fluxer`,
	},
	emailChangeRevert: {
		subject: 'Tvůj e-mail pro Fluxer byl změněn',
		body: `Ahoj {username},

E-mail tvého účtu Fluxer byl změněn na {newEmail}.

Pokud jsi změnu udělal(a) ty, nic dalšího není potřeba. Pokud ne, můžeš ji vrátit zpět a zabezpečit účet pomocí tohoto odkazu:

{revertUrl}

Tím se obnoví tvůj původní e-mail, odhlásíš se všude, odstraní se propojená telefonní čísla, vypne se MFA a bude nutné nastavit nové heslo.

- Tým zabezpečení Fluxer`,
	},
};

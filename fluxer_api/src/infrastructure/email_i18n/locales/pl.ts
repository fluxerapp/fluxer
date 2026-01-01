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

export const pl: EmailTranslations = {
	passwordReset: {
		subject: 'Zresetuj swoje hasło do Fluxer',
		body: `Cześć {username},

Otrzymaliśmy prośbę o zresetowanie hasła do Twojego konta Fluxer. Kliknij poniższy link, aby ustawić nowe hasło:

{resetUrl}

Jeśli nie prosiłeś(-aś) o reset hasła, możesz bezpiecznie zignorować tę wiadomość.

Ten link wygaśnie za 1 godzinę.

- Zespół Fluxer`,
	},
	emailVerification: {
		subject: 'Zweryfikuj swój adres e-mail w Fluxer',
		body: `Cześć {username},

Kliknij poniższy link, aby zweryfikować adres e-mail powiązany z Twoim kontem Fluxer:

{verifyUrl}

Jeśli nie utworzyłeś(-aś) konta Fluxer, możesz zignorować tę wiadomość.

Ten link wygaśnie za 24 godziny.

- Zespół Fluxer`,
	},
	ipAuthorization: {
		subject: 'Autoryzuj logowanie z nowego adresu IP',
		body: `Cześć {username},

Wykryliśmy próbę logowania do Twojego konta Fluxer z nowego adresu IP:

Adres IP: {ipAddress}
Lokalizacja: {location}

Jeśli to Ty, kliknij poniższy link, aby autoryzować ten adres IP:

{authUrl}

Jeśli to nie Ty próbowałeś(-aś) się zalogować, zalecamy natychmiastową zmianę hasła.

Ten link autoryzacyjny wygaśnie za 30 minut.

- Zespół Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'Twoje konto Fluxer zostało tymczasowo wyłączone',
		body: `Cześć {username},

Twoje konto Fluxer zostało tymczasowo wyłączone z powodu podejrzanej aktywności.

{reason, select,
	null {}
	other {Powód: {reason}

}}Aby odzyskać dostęp do konta, musisz zresetować hasło:

{forgotUrl}

Po zresetowaniu hasła będziesz mógł(-a) ponownie się zalogować.

Jeśli uważasz, że doszło do pomyłki, skontaktuj się z naszym zespołem wsparcia.

- Zespół Bezpieczeństwa Fluxer`,
	},
	accountTempBanned: {
		subject: 'Twoje konto Fluxer zostało tymczasowo zawieszone',
		body: `Cześć {username},

Twoje konto Fluxer zostało tymczasowo zawieszone za naruszenie Regulaminu lub Wytycznych Społeczności.

Czas trwania: {durationHours, plural,
	=1 {1 godzina}
	other {# godzin}
}
Zawieszone do: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Powód: {reason}}
}

W tym czasie nie będziesz mieć dostępu do konta.

Zachęcamy do zapoznania się z:
- Regulaminem: {termsUrl}
- Wytycznymi Społeczności: {guidelinesUrl}

Jeśli uważasz, że decyzja jest błędna lub niesprawiedliwa, możesz wysłać odwołanie na adres appeals@fluxer.app z tego adresu e-mail.  
Wyjaśnij dokładnie, dlaczego uważasz, że decyzja była niewłaściwa. Przeanalizujemy Twoje odwołanie i odpowiemy z decyzją.

- Zespół Bezpieczeństwa Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'Twoje konto Fluxer jest zaplanowane do usunięcia',
		body: `Cześć {username},

Twoje konto Fluxer zostało zakwalifikowane do trwałego usunięcia z powodu naruszenia Regulaminu lub Wytycznych Społeczności.

Planowana data usunięcia: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Powód: {reason}}
}

To poważne działanie. Wszystkie dane konta zostaną trwale usunięte w określonym terminie.

Zachęcamy do zapoznania się z:
- Regulaminem: {termsUrl}
- Wytycznymi Społeczności: {guidelinesUrl}

PROCES ODWOŁANIA:
Jeśli uważasz, że decyzja jest błędna lub niesprawiedliwa, masz 30 dni na złożenie odwołania na adres appeals@fluxer.app z tego adresu e-mail.

W odwołaniu:
- Wyjaśnij, dlaczego decyzja Twoim zdaniem jest błędna
- Podaj wszelkie istotne dowody lub kontekst

Członek zespołu bezpieczeństwa Fluxer przeanalizuje odwołanie i może wstrzymać usunięcie do czasu wydania ostatecznej decyzji.

- Zespół Bezpieczeństwa Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'Usunięcie Twojego konta Fluxer zostało zaplanowane',
		body: `Cześć {username},

Przykro nam, że odchodzisz! Usunięcie Twojego konta Fluxer zostało zaplanowane.

Planowana data usunięcia: {deletionDate, date, full} {deletionDate, time, short}

WAŻNE: Możesz anulować usunięcie konta w dowolnym momencie przed {deletionDate, date, full} {deletionDate, time, short}, logując się ponownie.

ZANIM ODEJDZIESZ:
Panel Prywatności w ustawieniach konta pozwala na:
- Usuwanie swoich wiadomości na platformie
- Eksportowanie ważnych danych przed odejściem

Uwaga: Po usunięciu konta nie będzie można usunąć wiadomości. Jeśli chcesz je usunąć, zrób to przed finalizacją usunięcia konta.

Jeśli zmienisz zdanie, po prostu zaloguj się ponownie, aby anulować usunięcie.

- Zespół Fluxer`,
	},
	inactivityWarning: {
		subject: 'Twoje konto Fluxer zostanie usunięte z powodu nieaktywności',
		body: `Cześć {username},

Zauważyliśmy, że nie logowałeś(-aś) się na swoje konto Fluxer od ponad 2 lat.

Ostatnie logowanie: {lastActiveDate, date, full} {lastActiveDate, time, short}

Zgodnie z naszą polityką przechowywania danych, nieaktywne konta są automatycznie kwalifikowane do usunięcia.

Planowana data usunięcia: {deletionDate, date, full} {deletionDate, time, short}

JAK ZACHOWAĆ KONTO:
Wystarczy zalogować się na {loginUrl} przed datą usunięcia. Nie trzeba wykonywać żadnych dodatkowych czynności.

JEŚLI SIĘ NIE ZALOGUJESZ:
- Twoje konto oraz wszystkie powiązane dane zostaną trwale usunięte
- Twoje wiadomości zostaną zanonimizowane („Usunięty użytkownik”)
- Tego działania nie można cofnąć

CHCESZ USUNĄĆ SWOJE WIADOMOŚCI?
Zaloguj się i użyj Panelu Prywatności przed usunięciem konta.

Mamy nadzieję, że jeszcze wrócisz na Fluxer!

- Zespół Fluxer`,
	},
	harvestCompleted: {
		subject: 'Twój eksport danych Fluxer jest gotowy',
		body: `Cześć {username},

Eksport Twoich danych został ukończony i jest gotowy do pobrania!

Podsumowanie eksportu:
- Liczba wiadomości: {totalMessages, number}
- Rozmiar pliku: {fileSizeMB} MB
- Format: archiwum ZIP z plikami JSON

Pobierz swoje dane: {downloadUrl}

WAŻNE: Ten link wygaśnie dnia {expiresAt, date, full} o {expiresAt, time, short}

Eksport zawiera:
- Wszystkie Twoje wiadomości posortowane według kanałów
- Metadane kanałów
- Informacje o profilu i koncie
- Przynależność do gildii oraz ustawienia
- Sesje uwierzytelniające i dane bezpieczeństwa

Dane są zapisane w formacie JSON, aby ułatwić analizę.

W razie pytań napisz na support@fluxer.app

- Zespół Fluxer`,
	},
	unbanNotification: {
		subject: 'Zawieszenie Twojego konta Fluxer zostało zniesione',
		body: `Cześć {username},

Dobre wieści! Zawieszenie Twojego konta Fluxer zostało zniesione.

Powód: {reason}

Możesz ponownie zalogować się i korzystać z Fluxer.

- Zespół Bezpieczeństwa Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'Twoje konto Fluxer jest zaplanowane do usunięcia',
		body: `Cześć {username},

Twoje konto Fluxer zostało zakwalifikowane do trwałego usunięcia.

Data usunięcia: {deletionDate, date, full} {deletionDate, time, short}
Powód: {reason}

Jest to poważne działanie. Twoje dane zostaną usunięte w podanym terminie.

Jeśli uważasz, że decyzja jest błędna, możesz wysłać odwołanie na appeals@fluxer.app

- Zespół Bezpieczeństwa Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'Twój prezent Fluxer Premium został cofnięty',
		body: `Cześć {username},

Informujemy, że Twój prezent Fluxer Premium został cofnięty z powodu sporu dotyczącego płatności (chargeback), zgłoszonego przez pierwotnego nabywcę.

Korzyści premium zostały usunięte z Twojego konta, ponieważ płatność została cofnięta.

W razie pytań napisz na support@fluxer.app

- Zespół Fluxer`,
	},
	reportResolved: {
		subject: 'Twoje zgłoszenie do Fluxer zostało rozpatrzone',
		body: `Cześć {username},

Twoje zgłoszenie (ID: {reportId}) zostało przeanalizowane przez nasz Zespół Bezpieczeństwa.

Odpowiedź Zespołu Bezpieczeństwa:
{publicComment}

Dziękujemy za pomoc w utrzymaniu bezpieczeństwa na Fluxer. Doceniamy Twój wkład w rozwój naszej społeczności.

W razie pytań lub wątpliwości skontaktuj się: safety@fluxer.app

- Zespół Bezpieczeństwa Fluxer`,
	},
	dsaReportVerification: {
		subject: 'Zweryfikuj swój e-mail dla zgłoszenia DSA',
		body: `Witaj,

Użyj następującego kodu weryfikacyjnego, aby przesłać zgłoszenie zgodnie z ustawą o usługach cyfrowych (Digital Services Act) na Fluxer:

{code}

Ten kod wygasa {expiresAt, date, full} o {expiresAt, time, short}.

Jeśli tego nie prosiłeś(-aś), zignoruj ten e-mail.

- Zespół Bezpieczeństwa Fluxer`,
	},
	registrationApproved: {
		subject: 'Twoja rejestracja w Fluxer została zatwierdzona',
		body: `Cześć {username},

Świetne wieści! Twoja rejestracja w Fluxer została zatwierdzona.

Możesz teraz zalogować się do aplikacji Fluxer tutaj:
{channelsUrl}

Witamy w społeczności Fluxer!

- Zespół Fluxer`,
	},
	emailChangeRevert: {
		subject: 'Twój e-mail Fluxer został zmieniony',
		body: `Cześć {username},

E-mail Twojego konta Fluxer zmieniono na {newEmail}.

Jeśli to Ty wprowadziłeś tę zmianę, nic nie musisz robić. Jeśli nie, możesz ją cofnąć i zabezpieczyć konto tym linkiem:

{revertUrl}

To przywróci poprzedni e-mail, wyloguje Cię z każdego miejsca, usunie powiązane numery telefonów, wyłączy MFA i wymusi ustawienie nowego hasła.

- Zespół Bezpieczeństwa Fluxer`,
	},
};

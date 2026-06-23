// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_PL_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Twoje konto {product_name} zostało tymczasowo wyłączone",
		"body": "Witaj {username},\n\nTymczasowo wyłączyliśmy Twoje konto {product_name}, ponieważ wykryliśmy podejrzaną aktywność.\n\n{reason, select,\n  null {}\n  other {Powód: {reason}}\n}\n\nAby odzyskać dostęp do konta, musisz zresetować hasło:\n\n{forgotUrl}\n\nPo zresetowaniu hasła będziesz mógł ponownie się zalogować.\n\nJeśli uważasz, że to błąd, skontaktuj się z naszym zespołem wsparcia.\n\n– Zespół Bezpieczeństwa {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "Twoje konto {product_name} zostanie usunięte na zawsze",
		"body": "Witaj {username},\n\nTwoje konto {product_name} zostało zaplanowane do trwałego usunięcia z powodu naruszenia naszych warunków usługi lub wytycznych społeczności.\n\nZaplanowane usunięcie: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Powód: {reason}}\n}\n\nJest to poważna decyzja. Dane Twojego konta zostaną usunięte na stałe w zaplanowanym terminie.\n\nSprawdź:\n- Warunki usługi: {termsUrl}\n- Wytyczne społeczności: {guidelinesUrl}\n\nProces odwoławczy:\nJeśli uważasz, że ta decyzja była błędna lub nieuzasadniona, masz 60 dni na złożenie odwołania. Wyślij e-mail na adres {appeals_email} z tego adresu e-mail.\n\nW swoim odwołaniu:\n- Wyjaśnij jasno, dlaczego uważasz, że decyzja była błędna lub nieuzasadniona\n- Przedstaw wszelkie istotne dowody lub kontekst\n\nCzłonek Zespołu Bezpieczeństwa {product_name} rozpatrzy Twoje odwołanie i może wstrzymać usunięcie do czasu podjęcia ostatecznej decyzji.\n\n– Zespół Bezpieczeństwa {product_name}"
	},
	"account_temp_banned": {
		"subject": "Twoje konto {product_name} zostało tymczasowo zawieszone",
		"body": "Witaj {username},\n\nTwoje konto {product_name} zostało tymczasowo zawieszone za naruszenie naszych warunków usługi lub wytycznych społeczności.\n\nCzas trwania: {durationHours, plural,\n  one {1 godzina} few {# godziny}\n  many {# godzin} other {# godzin}\n}\nZawieszone do: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Powód: {reason}}\n}\n\nW tym czasie nie będziesz mieć dostępu do swojego konta.\n\nSprawdź:\n- Warunki usługi: {termsUrl}\n- Wytyczne społeczności: {guidelinesUrl}\n\nJeśli uważasz, że ta decyzja była błędna lub nieuzasadniona, możesz złożyć odwołanie. Wyślij e-mail na adres {appeals_email} z tego adresu e-mail i jasno wyjaśnij, dlaczego uważasz, że decyzja była błędna. Rozpatrzymy Twoje odwołanie i odpowiemy z naszą decyzją.\n\n– Zespół Bezpieczeństwa {product_name}"
	},
	"donation_confirmation": {
		"subject": "Dziękujemy za darowiznę dla {product_name}",
		"body": "Witaj,\n\nDziękujemy za darowiznę dla {product_name}! Twoja {interval, select,\n  month {cykliczna darowizna}\n  year {cykliczna darowizna}\n  other {jednorazowa darowizna}\n} została {interval, select,\n  month {ustawiona}\n  year {ustawiona}\n  other {przetworzona}\n} pomyślnie.\n\nSzczegóły darowizny:\nKwota: {amount} {currency} {interval, select,\n  month {miesięcznie}\n  year {rocznie}\n  other {}\n}\n\nStripe wkrótce wyśle Ci osobną wiadomość e-mail z potwierdzeniem i fakturą w formacie PDF. Zawiera wszystkie szczegóły płatności i może służyć do celów podatkowych.\n\nMożesz przeglądać historię swoich darowizn, pobierać faktury {interval, select,\n  month {oraz zarządzać lub anulować subskrypcję}\n  year {oraz zarządzać lub anulować subskrypcję}\n  other {oraz zarządzać przyszłymi darowiznami}\n} w dowolnym momencie, korzystając z tego linku:\n\n{manageUrl}\n\nTwoje wsparcie pomaga utrzymać {product_name}. Dziękujemy!\n\n– Zespół {product_name}"
	},
	"donation_magic_link": {
		"subject": "Zarządzaj darowiznami dla {product_name}",
		"body": "Witaj,\n\nKliknij poniższy link, aby przejść do portalu darczyńcy:\n\n{manageUrl}\n\nW portalu możesz zarządzać subskrypcjami, pobierać faktury i przeglądać historię swoich darowizn.\n\nTen link wygasa {expiresAt, date, full} o {expiresAt, time, short}.\n\nJeśli nie prosiłeś o ten link, możesz bezpiecznie zignorować tę wiadomość e-mail.\n\n– Zespół {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Zweryfikuj swój e-mail dla raportu DSA",
		"body": "Witaj,\n\nUżyj poniższego kodu weryfikacyjnego, aby przesłać swój raport DSA dotyczący {product_name}:\n\n{code}\n\nTen kod wygasa {expiresAt, date, full} o {expiresAt, time, short}.\n\nJeśli nie prosiłeś o ten kod, możesz zignorować tę wiadomość e-mail.\n\n– Zespół Bezpieczeństwa {product_name}"
	},
	"email_change_new": {
		"subject": "Zweryfikuj nowy adres e-mail dla {product_name}",
		"body": "Witaj {username},\n\nWprowadź ten kod w aplikacji, aby zweryfikować swój nowy adres e-mail dla {product_name}:\n\n{code}\n\nTen kod wygasa {expiresAt, date, full} o {expiresAt, time, short}.\n\nJeśli nie prosiłeś o ten kod, możesz zignorować tę wiadomość e-mail.\n\n– Zespół {product_name}"
	},
	"email_change_original": {
		"subject": "Potwierdź zmianę adresu e-mail w {product_name}",
		"body": "Witaj {username},\n\nOtrzymaliśmy prośbę o zmianę adresu e-mail Twojego konta {product_name}.\n\nAby potwierdzić tę zmianę, wprowadź ten kod w aplikacji:\n\n{code}\n\nTen kod wygasa {expiresAt, date, full} o {expiresAt, time, short}.\n\nJeśli nie prosiłeś o to, natychmiast zabezpiecz swoje konto.\n\n– Zespół {product_name}"
	},
	"email_change_revert": {
		"subject": "Twój adres e-mail w {product_name} został zmieniony",
		"body": "Witaj {username},\n\nTwój adres e-mail konta {product_name} został zmieniony na {newEmail}.\n\nJeśli to Ty dokonałeś tej zmiany, nie musisz nic robić. Jeśli nie, możesz cofnąć zmianę i zabezpieczyć swoje konto, korzystając z tego linku:\n\n{revertUrl}\n\nSpowoduje to przywrócenie poprzedniego adresu e-mail, wylogowanie ze wszystkich urządzeń, usunięcie powiązanych numerów telefonów, wyłączenie MFA i konieczność ustawienia nowego hasła.\n\n– Zespół Bezpieczeństwa {product_name}"
	},
	"email_verification": {
		"subject": "Zweryfikuj adres e-mail w {product_name}",
		"body": "Witaj {username},\n\nZweryfikuj adres e-mail swojego konta {product_name}, klikając poniższy link:\n\n{verifyUrl}\n\nJeśli nie utworzyłeś konta {product_name}, możesz bezpiecznie zignorować tę wiadomość e-mail.\n\nTen link jest ważny przez 24 godziny.\n\n– Zespół {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "Usunięto bonusy z wykorzystanego prezentu",
		"body": "Witaj {username},\n\nKod prezentowy, który został wykorzystany, został pierwotnie opłacony przez inną osobę. Płatność ta została następnie cofnięta (chargeback).\n\nZ tego powodu usunęliśmy bonusy, które zostały dodane do Twojego konta po wykorzystaniu prezentu.\n\nJeśli uważasz, że to pomyłka, skontaktuj się z naszym zespołem wsparcia i podaj wszelkie szczegóły dotyczące kodu prezentowego oraz daty jego wykorzystania.\n\n– Zespół {product_name}"
	},
	"harvest_completed": {
		"subject": "Twój eksport danych z {product_name} jest gotowy do pobrania",
		"body": "Witaj {username},\n\nTwój eksport danych jest gotowy.\n\nLink do pobrania:\n{downloadUrl}\n\nWiadomości uwzględnione: {totalMessages, number}\nRozmiar pliku: {fileSizeMB, number} MB\n\nTen link wygasa {expiresAt, date, full} o {expiresAt, time, short}.\n\nJeśli nie prosiłeś o ten eksport, natychmiast zmień hasło i skontaktuj się z naszym zespołem wsparcia.\n\n– Zespół {product_name}"
	},
	"inactivity_warning": {
		"subject": "Twoje konto {product_name} zostanie usunięte z powodu braku aktywności",
		"body": "Witaj {username},\n\nNie odnotowaliśmy żadnej aktywności na Twoim koncie {product_name} od {lastActiveDate, date, full}.\n\nJeśli nie zalogujesz się do {deletionDate, date, full} o {deletionDate, time, short}, Twoje konto zostanie usunięte na stałe z powodu nieaktywności.\n\nZaloguj się tutaj:\n{loginUrl}\n\nJeśli ostatnio korzystałeś z {product_name}, natychmiast skontaktuj się z naszym zespołem wsparcia.\n\n– Zespół {product_name}"
	},
	"ip_authorization": {
		"subject": "Autoryzuj logowanie z nowego adresu IP",
		"body": "Witaj {username},\n\nWykryliśmy próbę logowania do Twojego konta {product_name} z nowego adresu IP:\n\nAdres IP: {ipAddress}\nLokalizacja: {location}\n\nJeśli to Ty, autoryzuj ten adres IP, klikając poniższy link:\n\n{authUrl}\n\nJeśli nie próbowałeś się zalogować, natychmiast zmień hasło.\n\nTen link jest ważny przez 30 minut.\n\n– Zespół {product_name}"
	},
	"password_change_verification": {
		"subject": "Potwierdź zmianę hasła w {product_name}",
		"body": "Witaj {username},\n\nOtrzymaliśmy prośbę o zmianę hasła do Twojego konta {product_name}.\n\nAby potwierdzić tę zmianę, wprowadź ten kod w aplikacji:\n\n{code}\n\nTen kod wygasa o godzinie {expiresAt}.\n\nJeśli nie prosiłeś o to, ktoś może mieć dostęp do Twojego konta. Natychmiast zmień hasło i włącz uwierzytelnianie dwuskładnikowe.\n\n– Zespół {product_name}"
	},
	"password_reset": {
		"subject": "Zresetuj hasło w {product_name}",
		"body": "Witaj {username},\n\nPoprosiłeś o zresetowanie hasła w {product_name}. Użyj poniższego linku, aby ustawić nowe hasło:\n\n{resetUrl}\n\nJeśli nie prosiłeś o to, możesz bezpiecznie zignorować tę wiadomość e-mail.\n\nTen link jest ważny przez 1 godzinę.\n\n– Zespół {product_name}"
	},
	"registration_approved": {
		"subject": "Twoja rejestracja w {product_name} została zatwierdzona",
		"body": "Witaj {username},\n\nDobre wieści: Twoja rejestracja w {product_name} została zatwierdzona.\n\nMożesz teraz zalogować się do aplikacji {product_name} tutaj:\n{channelsUrl}\n\nWitaj w społeczności {product_name}.\n\n– Zespół {product_name}"
	},
	"report_resolved": {
		"subject": "Twoje zgłoszenie dotyczące {product_name} zostało rozpatrzone",
		"body": "Witaj {username},\n\nTwoje zgłoszenie (ID: {reportId}) zostało rozpatrzone przez nasz Zespół Bezpieczeństwa.{hasComment, select, yes {\n\nOdpowiedź od Zespołu Bezpieczeństwa:\n{publicComment}} other {}}\n\nDziękujemy za pomoc w utrzymaniu bezpieczeństwa {product_name} dla wszystkich. Traktujemy wszystkie zgłoszenia poważnie i doceniamy Twój wkład w społeczność.\n\nJeśli masz pytania lub wątpliwości dotyczące tego wyniku, skontaktuj się z {safety_email}.\n\n– Zespół Bezpieczeństwa {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Twoje konto {product_name} zostanie usunięte na zawsze",
		"body": "Witaj {username},\n\nTwoje konto {product_name} zostało zaplanowane do trwałego usunięcia.\n\nZaplanowane usunięcie: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Powód: {reason}}\n}\n\nJest to poważna decyzja. Dane Twojego konta zostaną usunięte na stałe w zaplanowanym terminie.\n\nJeśli uważasz, że ta decyzja była błędna, możesz złożyć odwołanie. Wyślij e-mail na adres {appeals_email} z tego adresu e-mail.\n\n– Zespół Bezpieczeństwa {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "Usunięcie Twojego konta {product_name} zostało zaplanowane",
		"body": "Witaj {username},\n\nPoprosiłeś o usunięcie konta {product_name}. Twoje konto jest zaplanowane do usunięcia na stałe na:\n\n{deletionDate, date, full} o {deletionDate, time, short}\n\nJeśli nie prosiłeś o to, zaloguj się na swoje konto, aby anulować usunięcie. Zalecamy również zmianę hasła w celu zabezpieczenia konta.\n\n– Zespół {product_name}"
	},
	"unban_notification": {
		"subject": "Zawieszenie Twojego konta {product_name} zostało zniesione",
		"body": "Witaj {username},\n\nDobre wieści: zawieszenie Twojego konta {product_name} zostało zniesione.\n\n{reason, select,\n  null {}\n  other {Powód: {reason}}\n}\n\nMożesz teraz ponownie się zalogować i kontynuować korzystanie z {product_name} jak zwykle.\n\n– Zespół Bezpieczeństwa {product_name}"
	}
});

export default EMAIL_I18N_PL_MESSAGES;

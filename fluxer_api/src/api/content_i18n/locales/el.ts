// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_EL_MESSAGES = defineContentI18nLocaleMessages({
	"auth.unknown_location": "Άγνωστη τοποθεσία",
	"billing.donation_description_monthly": "Μηνιαία δωρεά για την υποστήριξη του {product_name}",
	"billing.donation_description_one_time": "Εφάπαξ δωρεά για την υποστήριξη του {product_name}",
	"billing.donation_description_yearly": "Ετήσια δωρεά για την υποστήριξη του {product_name}",
	"billing.donation_name_one_time": "Δωρεά στο {product_name}",
	"billing.donation_name_recurring": "Επαναλαμβανόμενη δωρεά στο {product_name}",
	"billing.eu_withdrawal_waiver_checkout": "Αν είμαι καταναλωτής ΕΕ/ΕΟΧ, συναινώ ρητά στην άμεση παροχή του ψηφιακού περιεχομένου του πλάνου {premium_tier_name} του {product_name} και αναγνωρίζω ότι χάνω το νόμιμο δικαίωμα υπαναχώρησης μόλις παρασχεθεί η πρόσβαση. Αυτό δεν επηρεάζει άλλα υποχρεωτικά δικαιώματα των καταναλωτών. Ανατρέξτε στους [Όρους Παροχής Υπηρεσιών]({terms_url}).",
	"bulk_message_deletion.complete": "Ολοκληρώσαμε τη διαγραφή των μηνυμάτων σου. Αφαιρέσαμε {message_count, plural, =0 {0 μηνύματα} one {# μήνυμα} other {# μηνύματα}} από {channel_count, plural, =0 {0 τοποθεσίες} one {# τοποθεσία} other {# τοποθεσίες}}.",
	"content.virus_detected": "Αυτό το αρχείο επισημάνθηκε ως δυνητικά μη ασφαλές και έχει αφαιρεθεί.",
	"guild.bulk_create.emoji_limit": "Επιτεύχθηκε το μέγιστο όριο emoji ({limit}).",
	"guild.bulk_create.sticker_limit": "Επιτεύχθηκε το μέγιστο όριο αυτοκόλλητων ({limit}).",
	"guild.bulk_create.unknown_error": "Άγνωστο σφάλμα.",
	"guild.default_category_text": "Κανάλια κειμένου",
	"guild.default_category_voice": "Φωνητικά κανάλια",
	"guild.default_channel_text": "γενικό",
	"guild.default_channel_voice": "Γενικό"
});

export default CONTENT_I18N_EL_MESSAGES;

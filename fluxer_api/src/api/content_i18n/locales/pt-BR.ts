// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineContentI18nLocaleMessages} from '../ContentI18nMessages';

const CONTENT_I18N_PT_BR_MESSAGES = defineContentI18nLocaleMessages({
	"auth.unknown_location": "Local desconhecido",
	"billing.donation_description_monthly": "Doação mensal para apoiar o {product_name}",
	"billing.donation_description_one_time": "Doação única para apoiar o {product_name}",
	"billing.donation_description_yearly": "Doação anual para apoiar o {product_name}",
	"billing.donation_name_one_time": "Doação para o {product_name}",
	"billing.donation_name_recurring": "Doação recorrente para o {product_name}",
	"billing.eu_withdrawal_waiver_checkout": "Se eu for um consumidor da UE/EEE, consinto expressamente que o conteúdo digital {product_name} {premium_tier_name} seja fornecido imediatamente e reconheço que perco meu direito legal de arrependimento assim que o acesso for concedido. Isso não afeta outros direitos obrigatórios do consumidor. Consulte os [Termos de Serviço]({terms_url}).",
	"bulk_message_deletion.complete": "Terminamos de excluir suas mensagens. Removemos {message_count, plural, =0 {0 mensagens} one {# mensagem} other {# mensagens}} de {channel_count, plural, =0 {0 lugares} one {# lugar} other {# lugares}}.",
	"content.virus_detected": "Esse arquivo foi sinalizado como potencialmente inseguro e foi removido.",
	"guild.bulk_create.emoji_limit": "Número máximo de emojis atingido ({limit}).",
	"guild.bulk_create.sticker_limit": "Número máximo de figurinhas atingido ({limit}).",
	"guild.bulk_create.unknown_error": "Erro desconhecido.",
	"guild.default_category_text": "Canais de texto",
	"guild.default_category_voice": "Canais de voz",
	"guild.default_channel_text": "geral",
	"guild.default_channel_voice": "Geral"
});

export default CONTENT_I18N_PT_BR_MESSAGES;

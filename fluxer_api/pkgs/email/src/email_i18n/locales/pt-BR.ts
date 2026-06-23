// SPDX-License-Identifier: AGPL-3.0-or-later

import {defineEmailI18nLocaleMessages} from '../EmailI18nMessages';

const EMAIL_I18N_PT_BR_MESSAGES = defineEmailI18nLocaleMessages({
	"account_disabled_suspicious": {
		"subject": "Sua conta do {product_name} foi desativada temporariamente",
		"body": "Olá, {username},\n\nDesativamos temporariamente sua conta do {product_name} porque detectamos atividades suspeitas.\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nPara acessar sua conta novamente, você precisará redefinir sua senha:\n\n{forgotUrl}\n\nDepois de redefinir sua senha, você poderá entrar novamente.\n\nSe você acredita que isso foi um erro, entre em contato com nossa equipe de suporte.\n\n– Equipe de Segurança do {product_name}"
	},
	"account_scheduled_deletion": {
		"subject": "Sua conta do {product_name} será excluída permanentemente",
		"body": "Olá, {username},\n\nSua conta do {product_name} foi agendada para exclusão permanente devido a violações de nossos Termos de Serviço ou Diretrizes da Comunidade.\n\nExclusão agendada: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nEsta é uma medida séria. Os dados da sua conta serão excluídos permanentemente na data agendada.\n\nPor favor, revise:\n- Termos de Serviço: {termsUrl}\n- Diretrizes da Comunidade: {guidelinesUrl}\n\nProcesso de recurso:\nSe você acredita que esta decisão foi incorreta ou injustificada, você tem 60 dias para enviar um recurso. Envie um e-mail para {appeals_email} a partir deste endereço de e-mail.\n\nEm seu recurso:\n- Explique claramente por que você acredita que a decisão foi incorreta ou injustificada\n- Forneça quaisquer evidências ou contexto relevantes\n\nUm membro da Equipe de Segurança do {product_name} revisará seu recurso e poderá pausar a exclusão pendente até que uma decisão final seja alcançada.\n\n– Equipe de Segurança do {product_name}"
	},
	"account_temp_banned": {
		"subject": "Sua conta do {product_name} foi suspensa temporariamente",
		"body": "Olá, {username},\n\nSua conta do {product_name} foi suspensa temporariamente por violar nossos Termos de Serviço ou Diretrizes da Comunidade.\n\nDuração: {durationHours, plural,\n  =1 {1 hora}\n  other {# horas}\n}\nSuspensa até: {bannedUntil, date, full} {bannedUntil, time, short}\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nDurante este período, você não poderá acessar sua conta.\n\nPor favor, revise:\n- Termos de Serviço: {termsUrl}\n- Diretrizes da Comunidade: {guidelinesUrl}\n\nSe você acredita que esta decisão foi incorreta ou injustificada, você pode enviar um recurso. Envie um e-mail para {appeals_email} a partir deste endereço de e-mail e explique claramente por que você acredita que a decisão foi incorreta. Revisaremos seu recurso e responderemos com nossa decisão.\n\n– Equipe de Segurança do {product_name}"
	},
	"donation_confirmation": {
		"subject": "Agradecemos pela sua doação ao {product_name}",
		"body": "Olá,\n\nAgradecemos pela sua doação ao {product_name}! Sua {interval, select,\n  month {doação recorrente}\n  year {doação recorrente}\n  other {doação única}\n} foi {interval, select,\n  month {configurada}\n  year {configurada}\n  other {processada}\n} com sucesso.\n\nDetalhes da doação:\nValor: {amount} {currency} {interval, select,\n  month {por mês}\n  year {por ano}\n  other {}\n}\n\nA Stripe enviará um recibo separado com sua fatura em PDF em breve. Isso inclui todos os detalhes de pagamento e pode ser usado para fins fiscais.\n\nVocê pode visualizar seu histórico de doações, baixar faturas, {interval, select,\n  month {e gerenciar ou cancelar sua assinatura}\n  year {e gerenciar ou cancelar sua assinatura}\n  other {e gerenciar futuras doações}\n} a qualquer momento usando este link:\n\n{manageUrl}\n\nSeu apoio ajuda a manter o {product_name} funcionando. Agradecemos!\n\n– Equipe do {product_name}"
	},
	"donation_magic_link": {
		"subject": "Gerenciar suas doações do {product_name}",
		"body": "Olá,\n\nClique no link abaixo para acessar seu portal de doadores:\n\n{manageUrl}\n\nNo portal, você pode gerenciar assinaturas, baixar faturas e visualizar seu histórico de doações.\n\nEste link expira em {expiresAt, date, full} às {expiresAt, time, short}.\n\nSe você não solicitou este link, pode ignorar este e-mail com segurança.\n\n– Equipe do {product_name}"
	},
	"dsa_report_verification": {
		"subject": "Verifique seu e-mail para um relatório do DSA",
		"body": "Olá,\n\nUse o código de verificação abaixo para enviar seu relatório do Digital Services Act para o {product_name}:\n\n{code}\n\nEste código expira em {expiresAt, date, full} às {expiresAt, time, short}.\n\nSe você não solicitou isso, pode ignorar este e-mail.\n\n– Equipe de Segurança do {product_name}"
	},
	"email_change_new": {
		"subject": "Verifique seu novo e-mail do {product_name}",
		"body": "Olá, {username},\n\nInsira este código no aplicativo para verificar seu novo e-mail do {product_name}:\n\n{code}\n\nEste código expira em {expiresAt, date, full} às {expiresAt, time, short}.\n\nSe você não solicitou isso, pode ignorar este e-mail.\n\n– Equipe do {product_name}"
	},
	"email_change_original": {
		"subject": "Confirme a alteração do e-mail da sua conta do {product_name}",
		"body": "Olá, {username},\n\nRecebemos uma solicitação para alterar o endereço de e-mail da sua conta do {product_name}.\n\nPara confirmar esta alteração, insira este código no aplicativo:\n\n{code}\n\nEste código expira em {expiresAt, date, full} às {expiresAt, time, short}.\n\nSe você não solicitou isso, por favor, proteja sua conta imediatamente.\n\n– Equipe do {product_name}"
	},
	"email_change_revert": {
		"subject": "O e-mail da sua conta do {product_name} foi alterado",
		"body": "Olá, {username},\n\nO endereço de e-mail da sua conta do {product_name} foi alterado para {newEmail}.\n\nSe você fez essa alteração, nenhuma ação é necessária. Se você não fez, pode reverter a alteração e proteger sua conta usando este link:\n\n{revertUrl}\n\nIsso restaurará seu e-mail anterior, desconectará você de todos os lugares, removerá números de telefone vinculados, desativará a MFA e exigirá que você defina uma nova senha.\n\n– Equipe de Segurança do {product_name}"
	},
	"email_verification": {
		"subject": "Verifique o endereço de e-mail da sua conta do {product_name}",
		"body": "Olá, {username},\n\nPor favor, verifique o endereço de e-mail da sua conta do {product_name} clicando no link abaixo:\n\n{verifyUrl}\n\nSe você não criou uma conta do {product_name}, pode ignorar este e-mail com segurança.\n\nEste link é válido por 24 horas.\n\n– Equipe do {product_name}"
	},
	"gift_chargeback_notification": {
		"subject": "Vantagens do seu presente resgatado foram removidas",
		"body": "Olá, {username},\n\nUm código de presente que você resgatou foi originalmente pago por outra pessoa. Esse pagamento foi revertido (um estorno).\n\nPor causa disso, removemos os benefícios que foram adicionados à sua conta quando você resgatou o presente.\n\nSe você acha que isso é um erro, entre em contato com nossa equipe de suporte e inclua quaisquer detalhes que você tenha sobre o código do presente e quando você o resgatou.\n\n– Equipe do {product_name}"
	},
	"harvest_completed": {
		"subject": "Sua exportação de dados do {product_name} está pronta para download",
		"body": "Olá, {username},\n\nSua exportação de dados está pronta.\n\nLink para download:\n{downloadUrl}\n\nMensagens incluídas: {totalMessages, number}\nTamanho do arquivo: {fileSizeMB, number} MB\n\nEste link expira em {expiresAt, date, full} às {expiresAt, time, short}.\n\nSe você não solicitou esta exportação, por favor, altere sua senha imediatamente e entre em contato com nossa equipe de suporte.\n\n– Equipe do {product_name}"
	},
	"inactivity_warning": {
		"subject": "A sua conta do {product_name} será excluída devido à inatividade",
		"body": "Olá, {username},\n\nNão vimos nenhuma atividade em sua conta do {product_name} desde {lastActiveDate, date, full}.\n\nSe você não entrar até {deletionDate, date, full} às {deletionDate, time, short}, sua conta será excluída permanentemente devido à inatividade.\n\nEntre aqui:\n{loginUrl}\n\nSe você usou o {product_name} recentemente, entre em contato com nossa equipe de suporte imediatamente.\n\n– Equipe do {product_name}"
	},
	"ip_authorization": {
		"subject": "Autorizar login de um novo endereço IP",
		"body": "Olá, {username},\n\nDetectamos uma tentativa de login em sua conta do {product_name} de um novo endereço IP:\n\nEndereço IP: {ipAddress}\nLocalização: {location}\n\nSe foi você, por favor, autorize este endereço IP clicando no link abaixo:\n\n{authUrl}\n\nSe você não tentou entrar, por favor, altere sua senha imediatamente.\n\nEste link é válido por 30 minutos.\n\n– Equipe do {product_name}"
	},
	"password_change_verification": {
		"subject": "Confirme a alteração da senha da sua conta do {product_name}",
		"body": "Olá, {username},\n\nRecebemos uma solicitação para alterar a senha da sua conta do {product_name}.\n\nPara confirmar esta alteração, insira este código no aplicativo:\n\n{code}\n\nEste código expira em {expiresAt}.\n\nSe você não solicitou isso, alguém pode ter acesso à sua conta. Altere sua senha imediatamente e ative a autenticação de dois fatores.\n\n– Equipe do {product_name}"
	},
	"password_reset": {
		"subject": "Redefina a senha da sua conta do {product_name}",
		"body": "Olá, {username},\n\nVocê solicitou uma redefinição de senha da sua conta do {product_name}. Use o link abaixo para definir uma nova senha:\n\n{resetUrl}\n\nSe você não solicitou isso, pode ignorar este e-mail com segurança.\n\nEste link é válido por 1 hora.\n\n– Equipe do {product_name}"
	},
	"registration_approved": {
		"subject": "O seu registro do {product_name} foi aprovado",
		"body": "Olá, {username},\n\nBoas notícias: seu registro do {product_name} foi aprovado.\n\nAgora você pode entrar no aplicativo do {product_name} aqui:\n{channelsUrl}\n\nBem-vindo à comunidade do {product_name}.\n\n– Equipe do {product_name}"
	},
	"report_resolved": {
		"subject": "Sua denúncia no {product_name} foi analisada",
		"body": "Olá, {username},\n\nSua denúncia (ID: {reportId}) foi analisada por nossa Equipe de Segurança.{hasComment, select, yes {\n\nResposta da Equipe de Segurança:\n{publicComment}} other {}}\n\nAgradecemos por ajudar a manter o {product_name} seguro para todos. Levamos todas as denúncias a sério e agradecemos sua contribuição para a comunidade.\n\nSe você tiver alguma dúvida ou preocupação sobre este resultado, entre em contato com {safety_email}.\n\n– Equipe de Segurança do {product_name}"
	},
	"scheduled_deletion_notification": {
		"subject": "Sua conta do {product_name} será excluída permanentemente",
		"body": "Olá, {username},\n\nSua conta do {product_name} foi agendada para exclusão permanente.\n\nExclusão agendada: {deletionDate, date, full} {deletionDate, time, short}\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nEsta é uma medida séria. Os dados da sua conta serão excluídos permanentemente na data agendada.\n\nSe você acredita que esta decisão foi incorreta, você pode enviar um recurso. Envie um e-mail para {appeals_email} a partir deste endereço de e-mail.\n\n– Equipe de Segurança do {product_name}"
	},
	"self_deletion_scheduled": {
		"subject": "A exclusão da sua conta do {product_name} está agendada",
		"body": "Olá, {username},\n\nVocê solicitou a exclusão da sua conta do {product_name}. Sua conta está agendada para exclusão permanente em:\n\n{deletionDate, date, full} às {deletionDate, time, short}\n\nSe você não solicitou isso, entre em sua conta para cancelar a exclusão. Também recomendamos alterar sua senha para proteger sua conta.\n\n– Equipe do {product_name}"
	},
	"unban_notification": {
		"subject": "A suspensão da sua conta do {product_name} foi encerrada",
		"body": "Olá, {username},\n\nBoas notícias: a suspensão da sua conta do {product_name} foi encerrada.\n\n{reason, select,\n  null {}\n  other {Motivo: {reason}}\n}\n\nAgora você pode entrar novamente e continuar usando o {product_name} normalmente.\n\n– Equipe de Segurança do {product_name}"
	}
});

export default EMAIL_I18N_PT_BR_MESSAGES;

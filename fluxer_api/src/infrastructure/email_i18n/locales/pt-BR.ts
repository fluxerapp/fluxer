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

export const ptBR: EmailTranslations = {
	passwordReset: {
		subject: 'Redefina sua senha do Fluxer',
		body: `Olá {username},

Você solicitou a redefinição da senha da sua conta Fluxer. Clique no link abaixo para definir uma nova senha:

{resetUrl}

Se você não solicitou essa alteração, pode ignorar este e-mail com segurança.

Este link expira em 1 hora.

- Equipe Fluxer`,
	},
	emailVerification: {
		subject: 'Verifique seu endereço de e-mail do Fluxer',
		body: `Olá {username},

Por favor, verifique o endereço de e-mail da sua conta Fluxer clicando no link abaixo:

{verifyUrl}

Se você não criou uma conta Fluxer, basta ignorar este e-mail.

Este link expira em 24 horas.

- Equipe Fluxer`,
	},
	ipAuthorization: {
		subject: 'Autorize login de um novo endereço IP',
		body: `Olá {username},

Detectamos uma tentativa de login na sua conta Fluxer a partir de um novo endereço IP:

Endereço IP: {ipAddress}
Localização: {location}

Se foi você, autorize o acesso clicando no link abaixo:

{authUrl}

Se você não tentou fazer login, recomendamos que altere sua senha imediatamente.

Este link de autorização expira em 30 minutos.

- Equipe Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'Sua conta Fluxer foi temporariamente desativada',
		body: `Olá {username},

Sua conta Fluxer foi temporariamente desativada devido a atividade suspeita.

{reason, select,
	null {}
	other {Motivo: {reason}

}}Para recuperar o acesso à sua conta, você deve redefinir sua senha:

{forgotUrl}

Após redefinir sua senha, você poderá fazer login novamente.

Se acredita que isso foi um engano, entre em contato com nossa equipe de suporte.

- Equipe de Segurança Fluxer`,
	},
	accountTempBanned: {
		subject: 'Sua conta Fluxer foi temporariamente suspensa',
		body: `Olá {username},

Sua conta Fluxer foi temporariamente suspensa por violar nossos Termos de Serviço ou Diretrizes da Comunidade.

Duração: {durationHours, plural,
	=1 {1 hora}
	other {# horas}
}
Suspensa até: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {
Motivo: {reason}}
}

Durante a suspensão, você não poderá acessar sua conta.

Recomendamos revisar:
- Termos de Serviço: {termsUrl}
- Diretrizes da Comunidade: {guidelinesUrl}

Se acredita que essa decisão foi incorreta ou injusta, envie um recurso para appeals@fluxer.app usando este endereço de e-mail.  
Explique claramente por que acredita que a decisão está errada. Avaliaremos seu recurso e responderemos com nossa decisão.

- Equipe de Segurança Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'Sua conta Fluxer está programada para exclusão',
		body: `Olá {username},

Sua conta Fluxer foi programada para exclusão permanente devido a violações dos Termos de Serviço ou Diretrizes da Comunidade.

Data programada para exclusão: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {
Motivo: {reason}}
}

Esta é uma ação séria. Seus dados serão excluídos permanentemente na data programada.

Recomendamos revisar:
- Termos de Serviço: {termsUrl}
- Diretrizes da Comunidade: {guidelinesUrl}

PROCESSO DE RECURSO:
Se acredita que esta decisão foi incorreta ou injusta, você tem 30 dias para enviar um recurso para appeals@fluxer.app através deste e-mail.

No seu recurso:
- Explique por que acredita que a decisão foi equivocada
- Forneça qualquer evidência ou contexto relevante

Um membro da Equipe de Segurança Fluxer analisará o recurso e poderá suspender a exclusão até uma decisão final.

- Equipe de Segurança Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'A exclusão da sua conta Fluxer foi agendada',
		body: `Olá {username},

Sentimos muito em ver você partir! A exclusão da sua conta Fluxer foi agendada.

Data programada para exclusão: {deletionDate, date, full} {deletionDate, time, short}

IMPORTANTE: Você pode cancelar esta exclusão a qualquer momento antes de {deletionDate, date, full} {deletionDate, time, short}, simplesmente fazendo login novamente.

ANTES DE SAIR:
O Painel de Privacidade nas Configurações permite que você:
- Exclua suas mensagens da plataforma
- Exporte dados importantes antes de sair

Atenção: Após a exclusão da conta, não será mais possível excluir mensagens. Caso deseje excluir suas mensagens, faça isso antes da exclusão final.

Se mudar de ideia, basta fazer login novamente.

- Equipe Fluxer`,
	},
	inactivityWarning: {
		subject: 'Sua conta Fluxer será excluída por inatividade',
		body: `Olá {username},

Notamos que você não acessa sua conta Fluxer há mais de 2 anos.

Último acesso: {lastActiveDate, date, full} {lastActiveDate, time, short}

De acordo com nossa política de retenção de dados, contas inativas são automaticamente programadas para exclusão.

Data programada para exclusão: {deletionDate, date, full} {deletionDate, time, short}

COMO MANTER SUA CONTA:
Basta fazer login em {loginUrl} antes da data de exclusão. Não é necessário realizar mais nenhuma ação.

SE VOCÊ NÃO FIZER LOGIN:
- Sua conta e todos os dados serão permanentemente excluídos
- Suas mensagens serão anonimizadas (“Usuário excluído”)
- Esta ação não pode ser desfeita

QUER EXCLUIR SUAS MENSAGENS?
Faça login e utilize o Painel de Privacidade antes da exclusão.

Esperamos vê-lo novamente no Fluxer!

- Equipe Fluxer`,
	},
	harvestCompleted: {
		subject: 'Sua exportação de dados Fluxer está pronta',
		body: `Olá {username},

Sua exportação de dados foi concluída e está pronta para download!

Resumo da exportação:
- Total de mensagens: {totalMessages, number}
- Tamanho do arquivo: {fileSizeMB} MB
- Formato: Arquivo ZIP contendo arquivos JSON

Baixe seus dados: {downloadUrl}

IMPORTANTE: Este link expirará em {expiresAt, date, full} {expiresAt, time, short}

A exportação inclui:
- Todas as suas mensagens organizadas por canal
- Metadados dos canais
- Informações do seu perfil e conta
- Assinaturas de guilda e configurações
- Sessões de autenticação e dados de segurança

Os dados são fornecidos em formato JSON, facilitando a análise.

Dúvidas? Entre em contato via support@fluxer.app

- Equipe Fluxer`,
	},
	unbanNotification: {
		subject: 'A suspensão da sua conta Fluxer foi removida',
		body: `Olá {username},

Boas notícias! A suspensão da sua conta Fluxer foi removida.

Motivo: {reason}

Agora você pode acessar sua conta novamente e continuar usando o Fluxer.

- Equipe de Segurança Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'Sua conta Fluxer está programada para exclusão',
		body: `Olá {username},

Sua conta Fluxer foi agendada para exclusão permanente.

Data de exclusão: {deletionDate, date, full} {deletionDate, time, short}
Motivo: {reason}

Esta é uma ação séria e seus dados serão excluídos permanentemente.

Se acredita que esta decisão está incorreta, envie um recurso para appeals@fluxer.app.

- Equipe de Segurança Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'Seu presente Fluxer Premium foi revogado',
		body: `Olá {username},

Informamos que o presente Fluxer Premium que você resgatou foi revogado devido a uma disputa de pagamento (chargeback) realizada pelo comprador original.

Seus benefícios premium foram removidos da conta, pois o pagamento foi revertido.

Se tiver dúvidas, entre em contato via support@fluxer.app

- Equipe Fluxer`,
	},
	reportResolved: {
		subject: 'Sua denúncia no Fluxer foi analisada',
		body: `Olá {username},

Sua denúncia (ID: {reportId}) foi analisada pela nossa Equipe de Segurança.

Resposta da Equipe de Segurança:
{publicComment}

Obrigado por ajudar a manter o Fluxer seguro para todos. Agradecemos sua contribuição para a comunidade.

Se tiver dúvidas ou preocupações, entre em contato via safety@fluxer.app.

- Equipe de Segurança Fluxer`,
	},
	dsaReportVerification: {
		subject: 'Verifique seu e-mail para uma denúncia DSA',
		body: `Olá,

Use o seguinte código de verificação para enviar sua denúncia da Lei de Serviços Digitais no Fluxer:

{code}

Este código expira em {expiresAt, date, full} {expiresAt, time, short}.

Se você não solicitou isso, por favor ignore este e-mail.

- Equipe de Segurança Fluxer`,
	},
	registrationApproved: {
		subject: 'Seu cadastro no Fluxer foi aprovado',
		body: `Olá {username},

Boas notícias! Seu cadastro no Fluxer foi aprovado.

Você já pode acessar o aplicativo do Fluxer:
{channelsUrl}

Bem-vindo à comunidade Fluxer!

- Equipe Fluxer`,
	},
	emailChangeRevert: {
		subject: 'Seu e-mail da Fluxer foi alterado',
		body: `Olá {username},

O e-mail da sua conta Fluxer foi alterado para {newEmail}.

Se você fez essa alteração, nenhuma ação é necessária. Caso contrário, você pode reverter e proteger sua conta usando este link:

{revertUrl}

Isso restaurará seu e-mail anterior, encerrará suas sessões em todos os dispositivos, removerá telefones vinculados, desativará o MFA e exigirá uma nova senha.

- Equipe de Segurança da Fluxer`,
	},
};

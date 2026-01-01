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

import {deleteOneOrMany, fetchOne, upsertOne} from '~/database/Cassandra';
import type {EmailChangeTicketRow, EmailChangeTokenRow} from '~/database/CassandraTypes';
import {EmailChangeTickets, EmailChangeTokens} from '~/Tables';

const FETCH_TICKET_CQL = EmailChangeTickets.selectCql({
	where: EmailChangeTickets.where.eq('ticket'),
	limit: 1,
});

const FETCH_TOKEN_CQL = EmailChangeTokens.selectCql({
	where: EmailChangeTokens.where.eq('token_'),
	limit: 1,
});

export class EmailChangeRepository {
	async createTicket(row: EmailChangeTicketRow): Promise<void> {
		await upsertOne(EmailChangeTickets.insert(row));
	}

	async updateTicket(row: EmailChangeTicketRow): Promise<void> {
		await upsertOne(EmailChangeTickets.upsertAll(row));
	}

	async findTicket(ticket: string): Promise<EmailChangeTicketRow | null> {
		return await fetchOne<EmailChangeTicketRow>(FETCH_TICKET_CQL, {ticket});
	}

	async deleteTicket(ticket: string): Promise<void> {
		await deleteOneOrMany(EmailChangeTickets.deleteByPk({ticket}));
	}

	async createToken(row: EmailChangeTokenRow): Promise<void> {
		await upsertOne(EmailChangeTokens.insert(row));
	}

	async findToken(token: string): Promise<EmailChangeTokenRow | null> {
		return await fetchOne<EmailChangeTokenRow>(FETCH_TOKEN_CQL, {token_: token});
	}

	async deleteToken(token: string): Promise<void> {
		await deleteOneOrMany(EmailChangeTokens.deleteByPk({token_: token}));
	}
}

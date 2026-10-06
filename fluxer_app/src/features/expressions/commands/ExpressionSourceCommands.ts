// SPDX-License-Identifier: AGPL-3.0-or-later

import {Endpoints} from '@app/features/app/constants/Endpoints';
import type {ExpressionKind} from '@app/features/expressions/commands/ExpressionMetadataCommands';
import {http} from '@app/features/platform/transport/RestTransport';
import {failureCode} from '@app/features/platform/utils/ResponseInspection';
import {APIErrorCodes} from '@fluxer/constants/src/ApiErrorCodes';

export interface ExpressionSourceGuild {
	id: string;
	name: string;
	icon: string | null;
	features: Array<string>;
}

interface ExpressionSourceGuildResponse {
	id: string;
	name: string;
	icon?: string | null;
	features: Array<string>;
}

export type ExpressionSourceResult = {available: true; guild: ExpressionSourceGuild} | {available: false};

function sourceEndpoint(kind: ExpressionKind, id: string): string {
	return kind === 'emoji' ? Endpoints.EMOJI_SOURCE(id) : Endpoints.STICKER_SOURCE(id);
}

export async function fetchExpressionSource(kind: ExpressionKind, id: string): Promise<ExpressionSourceResult> {
	try {
		const response = await http.get<ExpressionSourceGuildResponse>(sourceEndpoint(kind, id));
		const body = response.body;
		return {
			available: true,
			guild: {id: body.id, name: body.name, icon: body.icon ?? null, features: body.features},
		};
	} catch (error) {
		if (failureCode(error) === APIErrorCodes.UNKNOWN_GUILD) {
			return {available: false};
		}
		throw error;
	}
}

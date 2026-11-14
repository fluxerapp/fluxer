// SPDX-License-Identifier: AGPL-3.0-or-later

import type {MessageEmbedField} from '@app/api/database/types/MessageTypes';

export class EmbedField {
	readonly name: string;
	readonly value: string;
	readonly inline: boolean;

	constructor(field: MessageEmbedField) {
		this.name = field.name ?? '';
		this.value = field.value ?? '';
		this.inline = field.inline ?? false;
	}

	toMessageEmbedField(): MessageEmbedField {
		return {
			name: this.name,
			value: this.value,
			inline: this.inline,
		};
	}
}

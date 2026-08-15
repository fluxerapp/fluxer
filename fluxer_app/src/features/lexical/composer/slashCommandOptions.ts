// SPDX-License-Identifier: AGPL-3.0-or-later

import type {CommandOption} from '@app/features/devtools/hooks/useCommands';

export interface PartitionedCommandOptions {
	required: Array<CommandOption>;
	optional: Array<CommandOption>;
}

export function partitionSlashCommandOptions(options: ReadonlyArray<CommandOption>): PartitionedCommandOptions {
	const required: Array<CommandOption> = [];
	const optional: Array<CommandOption> = [];
	for (const option of options) {
		if (option.required) {
			required.push(option);
		} else {
			optional.push(option);
		}
	}
	return {required, optional};
}

export function computeAbsentOptionalOptions(
	optional: ReadonlyArray<CommandOption>,
	presentNames: ReadonlySet<string> | ReadonlyArray<string>,
): Array<CommandOption> {
	const present = presentNames instanceof Set ? presentNames : new Set(presentNames);
	return optional.filter((option) => !present.has(option.name));
}

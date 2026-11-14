// SPDX-License-Identifier: AGPL-3.0-or-later

import * as GuildCommands from '@app/features/guild/commands/GuildCommands';
import {useState} from 'react';

export function useGuildFeatureToggle(guildId: string, feature: string, onError: (error: unknown) => void) {
	const [saving, setSaving] = useState(false);
	const setEnabled = async (enabled: boolean) => {
		if (saving) return;
		setSaving(true);
		try {
			await GuildCommands.toggleFeature(guildId, feature, enabled);
		} catch (error) {
			onError(error);
		} finally {
			setSaving(false);
		}
	};
	return {saving, setEnabled};
}

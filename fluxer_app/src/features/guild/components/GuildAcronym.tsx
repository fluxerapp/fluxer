// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/guild/components/GuildAcronym.module.css';
import {
	GUILD_ACRONYM_FALLBACK,
	getFittedGuildAcronymFontScale,
	getGuildAcronym,
} from '@app/features/guild/utils/GuildAcronymUtils';
import {clsx} from 'clsx';
import type React from 'react';
import {useMemo} from 'react';

interface GuildAcronymProps {
	readonly name: string;
	readonly className?: string;
	readonly 'data-flx'?: string;
}

type GuildAcronymStyle = React.CSSProperties & {
	'--guild-acronym-font-scale': string;
};

export function GuildAcronym({name, className, 'data-flx': dataFlx}: GuildAcronymProps) {
	const acronym = useMemo(() => getGuildAcronym(name) || GUILD_ACRONYM_FALLBACK, [name]);
	const style = useMemo<GuildAcronymStyle>(
		() => ({'--guild-acronym-font-scale': getFittedGuildAcronymFontScale(acronym).toFixed(4)}),
		[acronym],
	);
	return (
		<span className={clsx(styles.acronym, className)} style={style} data-flx={dataFlx ?? 'guild.guild-acronym.acronym'}>
			<span className={styles.text} data-flx="guild.guild-acronym.text">
				{acronym}
			</span>
		</span>
	);
}

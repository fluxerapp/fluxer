// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/lexical/composer/LexicalMessageComposer.module.css';
import {Plural} from '@lingui/react/macro';

interface SlashOptionalHintPillProps {
	remaining: number;
}

export const SlashOptionalHintPill = ({remaining}: SlashOptionalHintPillProps) => {
	return (
		<span
			className={styles.optionalHint}
			aria-hidden
			data-flx="lexical.composer.nodes.slash-optional-hint-pill.optional-hint"
		>
			<Plural
				value={remaining}
				one="+# more"
				other="+# more"
				data-flx="lexical.composer.nodes.slash-optional-hint-pill.plural"
			/>
		</span>
	);
};

// SPDX-License-Identifier: AGPL-3.0-or-later

import mentionRendererStyles from '@app/features/theme/styles/MentionRenderer.module.css';
import type React from 'react';

const ICON_LABEL_JOINER = '\u2060';

interface MentionLabelProps {
	icon?: React.ReactNode;
	children: React.ReactNode;
}

export function MentionLabel({icon, children}: MentionLabelProps): React.ReactElement {
	return (
		<>
			{icon}
			{icon == null ? null : ICON_LABEL_JOINER}
			<span className={mentionRendererStyles.label}>{children}</span>
		</>
	);
}

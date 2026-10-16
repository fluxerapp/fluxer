// SPDX-License-Identifier: AGPL-3.0-or-later

import {useLingui} from '@lingui/react';
import {cloneElement, Fragment, isValidElement, type ReactNode} from 'react';

function wrapTextSegments(node: ReactNode, key?: string): ReactNode {
	if (typeof node === 'string' || typeof node === 'number') {
		return (
			<flx-i18n key={key} data-flx="i18n.translation-safe-message.wrap-text-segments.flx-i18n">
				{node}
			</flx-i18n>
		);
	}
	if (Array.isArray(node)) {
		return node.map((child, index) => wrapTextSegments(child as ReactNode, `flx-i18n-${index}`));
	}
	if (isValidElement<{children?: ReactNode}>(node)) {
		if (typeof node.type !== 'string' && node.type !== Fragment) {
			return node;
		}
		const {children} = node.props;
		if (children == null) {
			return node;
		}
		return cloneElement(node, undefined, wrapTextSegments(children));
	}
	return node;
}

export function TranslationSafeMessage({children}: {children?: ReactNode}): ReactNode {
	const {i18n} = useLingui();
	return <Fragment key={i18n.locale}>{wrapTextSegments(children)}</Fragment>;
}

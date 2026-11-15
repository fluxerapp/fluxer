// SPDX-License-Identifier: AGPL-3.0-or-later

import styles from '@app/features/voice/components/IncomingCallManager.module.css';
import {useEffect, useState} from 'react';

let portalRoot: HTMLElement | null = null;

function createPortalRoot(): HTMLElement | null {
	if (portalRoot && document.body.contains(portalRoot)) {
		return portalRoot;
	}
	const root = document.createElement('div');
	root.className = styles.portalRoot;
	root.dataset.incomingCallPortal = 'true';
	root.dataset.floatingUiPortal = 'true';
	document.body.appendChild(root);
	portalRoot = root;
	return root;
}

export function useIncomingCallPortalRoot(): HTMLElement | null {
	const [root, setRoot] = useState<HTMLElement | null>(() => createPortalRoot());
	useEffect(() => {
		setRoot((prev) => prev ?? createPortalRoot());
	}, []);
	return root;
}

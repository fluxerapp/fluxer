// SPDX-License-Identifier: AGPL-3.0-or-later

import {resolveDownloadObjectPath} from '@app/api/download/DownloadRedirects';
import {describe, expect, it} from 'vitest';

describe('download object paths', () => {
	it('refuses a traversal attempt rather than pointing at another prefix', () => {
		expect(resolveDownloadObjectPath('/dl/desktop/../harvests/dump.zip')).toBeNull();
		expect(resolveDownloadObjectPath('/dl/../desktop/stable/linux/x64/latest')).toBeNull();
		expect(resolveDownloadObjectPath('/dl/desktop/stable/linux/x64/lat\0est')).toBeNull();
	});
});

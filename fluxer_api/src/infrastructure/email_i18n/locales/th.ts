/*
 * Copyright (C) 2026 Fluxer Contributors
 *
 * This file is part of Fluxer.
 *
 * Fluxer is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * Fluxer is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with Fluxer. If not, see <https://www.gnu.org/licenses/>.
 */

import type {EmailTranslations} from '../types';

export const th: EmailTranslations = {
	passwordReset: {
		subject: 'รีเซ็ตรหัสผ่าน Fluxer ของคุณ',
		body: `สวัสดี {username},

คุณได้ส่งคำขอรีเซ็ตรหัสผ่านสำหรับบัญชี Fluxer ของคุณ โปรดคลิกลิงก์ด้านล่างเพื่อกำหนดรหัสผ่านใหม่:

{resetUrl}

หากคุณไม่ได้ส่งคำขอนี้ คุณสามารถละเว้นอีเมลฉบับนี้ได้อย่างปลอดภัย

ลิงก์นี้จะหมดอายุภายใน 1 ชั่วโมง

- ทีมงาน Fluxer`,
	},
	emailVerification: {
		subject: 'ยืนยันที่อยู่อีเมล Fluxer ของคุณ',
		body: `สวัสดี {username},

โปรดยืนยันที่อยู่อีเมลสำหรับบัญชี Fluxer ของคุณโดยคลิกลิงก์ด้านล่าง:

{verifyUrl}

หากคุณไม่ได้สร้างบัญชี Fluxer คุณสามารถละเว้นอีเมลนี้ได้

ลิงก์นี้จะหมดอายุภายใน 24 ชั่วโมง

- ทีมงาน Fluxer`,
	},
	ipAuthorization: {
		subject: 'ยืนยันการเข้าสู่ระบบจาก IP ใหม่',
		body: `สวัสดี {username},

เราพบความพยายามเข้าสู่ระบบบัญชี Fluxer ของคุณจาก IP Address ใหม่:

IP Address: {ipAddress}
ตำแหน่ง: {location}

หากเป็นคุณ โปรดยืนยัน IP Address นี้โดยคลิกลิงก์ด้านล่าง:

{authUrl}

หากคุณไม่ได้พยายามเข้าสู่ระบบ โปรดเปลี่ยนรหัสผ่านทันที

ลิงก์สำหรับยืนยันนี้จะหมดอายุใน 30 นาที

- ทีมงาน Fluxer`,
	},
	accountDisabledSuspicious: {
		subject: 'บัญชี Fluxer ของคุณถูกปิดใช้งานชั่วคราว',
		body: `สวัสดี {username},

บัญชี Fluxer ของคุณถูกปิดใช้งานชั่วคราวเนื่องจากพบกิจกรรมที่น่าสงสัย

{reason, select,
	null {}
	other {สาเหตุ: {reason}

}}ในการกู้คืนการเข้าถึง คุณต้องรีเซ็ตรหัสผ่าน:

{forgotUrl}

หลังจากรีเซ็ตรหัสผ่านแล้ว คุณจะสามารถเข้าสู่ระบบได้อีกครั้ง

หากเชื่อว่าการดำเนินการนี้เกิดขึ้นโดยความผิดพลาด โปรดติดต่อทีมสนับสนุนของเรา

- ทีมความปลอดภัย Fluxer`,
	},
	accountTempBanned: {
		subject: 'บัญชี Fluxer ของคุณถูกระงับชั่วคราว',
		body: `สวัสดี {username},

บัญชี Fluxer ของคุณถูกระงับชั่วคราวเนื่องจากละเมิดข้อกำหนดการให้บริการหรือแนวทางชุมชนของเรา

ระยะเวลา: {durationHours, plural,
	=1 {1 ชั่วโมง}
	other {# ชั่วโมง}
}
ถูกระงับจนถึง: {bannedUntil, date, full} {bannedUntil, time, short}
{reason, select,
	null {}
	other {สาเหตุ: {reason}}
}

ในช่วงระงับนี้ คุณจะไม่สามารถเข้าถึงบัญชีของคุณได้

เราแนะนำให้ทบทวน:
- ข้อกำหนดการให้บริการ: {termsUrl}
- แนวทางชุมชน: {guidelinesUrl}

หากเชื่อว่ามีการบังคับใช้ที่ไม่ถูกต้อง คุณสามารถยื่นอุทธรณ์ได้ที่ appeals@fluxer.app โดยใช้อีเมลนี้  
โปรดอธิบายอย่างชัดเจนว่าทำไมคุณเชื่อว่าการตัดสินใจนั้นไม่ถูกต้อง ทีมงานของเราจะตรวจสอบและแจ้งผลให้คุณทราบ

- ทีมความปลอดภัย Fluxer`,
	},
	accountScheduledDeletion: {
		subject: 'บัญชี Fluxer ของคุณมีกำหนดลบ',
		body: `สวัสดี {username},

บัญชี Fluxer ของคุณถูกกำหนดให้ถูกลบอย่างถาวรเนื่องจากละเมิดข้อกำหนดการให้บริการหรือแนวทางชุมชน

วันที่กำหนดลบ: {deletionDate, date, full} {deletionDate, time, short}
{reason, select,
	null {}
	other {สาเหตุ: {reason}}
}

นี่เป็นมาตรการที่ร้ายแรง ข้อมูลบัญชีทั้งหมดของคุณจะถูกลบอย่างถาวรในวันที่กำหนด

โปรดทบทวน:
- ข้อกำหนดการให้บริการ: {termsUrl}
- แนวทางชุมชน: {guidelinesUrl}

ขั้นตอนการอุทธรณ์:
หากเชื่อว่าการตัดสินใจนี้ไม่ถูกต้อง คุณมีเวลา 30 วันในการส่งอุทธรณ์ไปยัง appeals@fluxer.app โดยใช้อีเมลนี้

ในอุทธรณ์ของคุณ โปรด:
- อธิบายอย่างชัดเจนว่าการบังคับใช้นี้ไม่ถูกต้องอย่างไร
- แนบหลักฐานหรือข้อมูลที่เกี่ยวข้อง

ทีมความปลอดภัย Fluxer จะตรวจสอบคำอุทธรณ์และอาจระงับการลบจนกว่าจะมีการตัดสินขั้นสุดท้าย

- ทีมความปลอดภัย Fluxer`,
	},
	selfDeletionScheduled: {
		subject: 'การลบบัญชี Fluxer ของคุณถูกตั้งเวลาไว้แล้ว',
		body: `สวัสดี {username},

เราเสียใจที่เห็นคุณจากไป! การลบบัญชี Fluxer ของคุณถูกตั้งเวลาไว้แล้ว

วันที่กำหนดลบ: {deletionDate, date, full} {deletionDate, time, short}

สำคัญ: คุณสามารถยกเลิกการลบนี้ได้ทุกเมื่อก่อน {deletionDate, date, full} {deletionDate, time, short} เพียงเข้าสู่ระบบอีกครั้ง

ก่อนที่คุณจะไป:
แดชบอร์ดความเป็นส่วนตัวในเมนูการตั้งค่าผู้ใช้ให้คุณสามารถ:
- ลบข้อความของคุณบนแพลตฟอร์ม
- ดาวน์โหลดข้อมูลสำคัญก่อนออกจากระบบ

โปรดทราบ: หลังจากบัญชีถูกลบ คุณจะไม่สามารถลบข้อความได้อีก หากต้องการลบ โปรดดำเนินการก่อนการลบเสร็จสมบูรณ์

หากคุณเปลี่ยนใจ เพียงเข้าสู่ระบบเพื่อยกเลิกการลบ

- ทีมงาน Fluxer`,
	},
	inactivityWarning: {
		subject: 'บัญชี Fluxer ของคุณจะถูกลบเนื่องจากไม่มีการใช้งาน',
		body: `สวัสดี {username},

เราสังเกตว่าคุณไม่ได้เข้าสู่ระบบบัญชี Fluxer มาเป็นเวลากว่า 2 ปี

การเข้าสู่ระบบครั้งล่าสุด: {lastActiveDate, date, full} {lastActiveDate, time, short}

ตามนโยบายการเก็บรักษาข้อมูลของเรา บัญชีที่ไม่มีการใช้งานจะถูกตั้งเวลาเพื่อลบโดยอัตโนมัติ

วันที่กำหนดลบ: {deletionDate, date, full} {deletionDate, time, short}

วิธีเก็บรักษาบัญชีของคุณ:
เพียงเข้าสู่ระบบที่ {loginUrl} ก่อนวันที่ลบ บัญชีของคุณจะไม่ถูกลบ

หากคุณไม่เข้าสู่ระบบ:
- บัญชีและข้อมูลทั้งหมดจะถูกลบถาวร
- ข้อความของคุณจะถูกทำให้ไม่ระบุตัวตน (“ผู้ใช้ที่ถูกลบ”)
- การกระทำนี้ไม่สามารถย้อนกลับได้

ต้องการลบข้อความของคุณหรือไม่?
เพียงเข้าสู่ระบบและใช้แดชบอร์ดความเป็นส่วนตัวก่อนกำหนดลบ

หวังว่าจะได้พบคุณอีกครั้งบน Fluxer!

- ทีมงาน Fluxer`,
	},
	harvestCompleted: {
		subject: 'ข้อมูลส่งออก Fluxer ของคุณพร้อมให้ดาวน์โหลดแล้ว',
		body: `สวัสดี {username},

การส่งออกข้อมูลของคุณเสร็จสมบูรณ์และพร้อมให้ดาวน์โหลดแล้ว!

สรุปข้อมูลที่ส่งออก:
- จำนวนข้อความทั้งหมด: {totalMessages, number}
- ขนาดไฟล์: {fileSizeMB} MB
- รูปแบบ: ไฟล์ ZIP ที่มี JSON

ดาวน์โหลดข้อมูลของคุณ: {downloadUrl}

สำคัญ: ลิงก์นี้จะหมดอายุใน {expiresAt, date, full} {expiresAt, time, short}

ข้อมูลที่ส่งออกประกอบด้วย:
- ข้อความทั้งหมดของคุณแบ่งตามช่อง
- ข้อมูลเมตาของช่อง
- โปรไฟล์และข้อมูลบัญชีของคุณ
- การเป็นสมาชิกกิลด์และการตั้งค่า
- เซสชันการยืนยันตัวตนและข้อมูลความปลอดภัย

ข้อมูลอยู่ในรูปแบบ JSON เพื่อความสะดวกในการวิเคราะห์

หากมีคำถาม โปรดติดต่อ support@fluxer.app

- ทีมงาน Fluxer`,
	},
	unbanNotification: {
		subject: 'การระงับบัญชี Fluxer ของคุณถูกยกเลิกแล้ว',
		body: `สวัสดี {username},

ข่าวดี! การระงับบัญชี Fluxer ของคุณถูกยกเลิกแล้ว

สาเหตุ: {reason}

คุณสามารถเข้าสู่ระบบและใช้ Fluxer ได้ตามปกติ

- ทีมความปลอดภัย Fluxer`,
	},
	scheduledDeletionNotification: {
		subject: 'บัญชี Fluxer ของคุณมีกำหนดลบ',
		body: `สวัสดี {username},

บัญชี Fluxer ของคุณมีกำหนดที่จะถูกลบอย่างถาวร

วันที่กำหนดลบ: {deletionDate, date, full} {deletionDate, time, short}
สาเหตุ: {reason}

นี่เป็นการดำเนินการที่ร้ายแรง ข้อมูลทั้งหมดของคุณจะถูกลบถาวรในวันดังกล่าว

หากคุณเชื่อว่าการตัดสินใจนี้ไม่ถูกต้อง สามารถยื่นอุทธรณ์ได้ที่ appeals@fluxer.app

- ทีมความปลอดภัย Fluxer`,
	},
	giftChargebackNotification: {
		subject: 'ของขวัญ Fluxer Premium ของคุณถูกเพิกถอน',
		body: `สวัสดี {username},

เราต้องการแจ้งให้คุณทราบว่าของขวัญ Fluxer Premium ที่คุณแลกรับถูกเพิกถอน เนื่องจากมีข้อพิพาทการชำระเงิน (chargeback) ที่ถูกยื่นโดยผู้ซื้อเดิม

สิทธิประโยชน์แบบพรีเมียมของคุณถูกนำออกจากบัญชีแล้ว เนื่องจากการชำระเงินถูกยกเลิก

หากมีคำถาม โปรดติดต่อ support@fluxer.app

- ทีมงาน Fluxer`,
	},
	reportResolved: {
		subject: 'รายงานของคุณบน Fluxer ได้รับการตรวจสอบแล้ว',
		body: `สวัสดี {username},

รายงานของคุณ (ID: {reportId}) ได้รับการตรวจสอบโดยทีมความปลอดภัยของเราแล้ว

คำตอบจากทีมความปลอดภัย:
{publicComment}

ขอบคุณที่ช่วยให้ Fluxer เป็นพื้นที่ที่ปลอดภัยสำหรับทุกคน เราขอขอบคุณในการมีส่วนร่วมของคุณต่อชุมชน

หากมีคำถามหรือข้อกังวล โปรดติดต่อ safety@fluxer.app

- ทีมความปลอดภัย Fluxer`,
	},
	dsaReportVerification: {
		subject: 'ยืนยันอีเมลของคุณสำหรับรายงาน DSA',
		body: `สวัสดี,

ใช้รหัสยืนยันต่อไปนี้เพื่อส่งรายงานตามพระราชบัญญัติบริการดิจิทัลบน Fluxer:

{code}

รหัสนี้จะหมดอายุใน {expiresAt, date, full} {expiresAt, time, short}

หากคุณไม่ได้ขอสิ่งนี้ โปรดเพิกเฉยต่ออีเมลนี้

- ทีมความปลอดภัย Fluxer`,
	},
	registrationApproved: {
		subject: 'การลงทะเบียน Fluxer ของคุณได้รับการอนุมัติแล้ว',
		body: `สวัสดี {username},

ข่าวดี! การลงทะเบียน Fluxer ของคุณได้รับการอนุมัติแล้ว

คุณสามารถเข้าสู่แอป Fluxer ได้ที่:
{channelsUrl}

ยินดีต้อนรับสู่ชุมชน Fluxer!

- ทีมงาน Fluxer`,
	},
	emailChangeRevert: {
		subject: 'อีเมล Fluxer ของคุณถูกเปลี่ยนแล้ว',
		body: `สวัสดี {username},

อีเมลของบัญชี Fluxer ของคุณถูกเปลี่ยนเป็น {newEmail}.

หากคุณเป็นผู้เปลี่ยน ไม่ต้องดำเนินการใด ๆ เพิ่มเติม หากไม่ใช่ คุณสามารถย้อนกลับและปกป้องบัญชีได้ผ่านลิงก์นี้:

{revertUrl}

การดำเนินการนี้จะกู้คืนอีเมลเดิมของคุณ ออกจากระบบทุกเซสชัน ลบหมายเลขโทรศัพท์ที่เชื่อมไว้ ปิดใช้งาน MFA และต้องตั้งรหัสผ่านใหม่

- ทีมความปลอดภัย Fluxer`,
	},
};

// https://github.com/torvalds/linux/blob/master/kernel/time/posix-clock.c
#include <linux/device.h>
#include <linux/export.h>
#include <linux/file.h>
#include <linux/posix-clock.h>
#include <linux/slab.h>
#include <linux/syscalls.h>
#include <linux/uaccess.h>

#include "posix-timers.h"

static void delete_clock(struct kref *kref);

/*
 * Returns NULL if the posix_clock instance attached to 'fp' is old and stale.
 */
static struct posix_clock *get_posix_clock(struct file *fp)
{
	struct posix_clock *clk = fp->private_data;

	down_read(&clk->rwsem);

	if (!clk->zombie)
		return clk;

	up_read(&clk->rwsem);

	return NULL;
}

static void put_posix_clock(struct posix_clock *clk)
{
	up_read(&clk->rwsem);
}

static ssize_t posix_clock_read(struct file *fp, char __user *buf,
				size_t count, loff_t *ppos)
{
	struct posix_clock *clk = get_posix_clock(fp);
	int err = -EINVAL;

	if (!clk)
		return -ENODEV;

	if (clk->ops.read)
		err = clk->ops.read(clk, fp->f_flags, buf, count);

	put_posix_clock(clk);

	return err;
}

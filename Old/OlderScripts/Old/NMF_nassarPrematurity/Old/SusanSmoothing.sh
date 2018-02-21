def create_susan_smooth(name="susan_smooth", separate_masks=True):
    """Create a SUSAN smoothing workflow
    Parameters
    ----------
    ::
        name : name of workflow (default: susan_smooth)
        separate_masks : separate masks for each run
    Inputs::
        inputnode.in_files : functional runs (filename or list of filenames)
        inputnode.fwhm : fwhm for smoothing with SUSAN
        inputnode.mask_file : mask used for estimating SUSAN thresholds (but not for smoothing)
    Outputs::
        outputnode.smoothed_files : functional runs (filename or list of filenames)
    Example
    -------
    >>> smooth = create_susan_smooth()
    >>> smooth.inputs.inputnode.in_files = 'f3.nii'
    >>> smooth.inputs.inputnode.fwhm = 5
    >>> smooth.inputs.inputnode.mask_file = 'mask.nii'
    >>> smooth.run() # doctest: +SKIP
    """

    susan_smooth = pe.Workflow(name=name)

    """
    Set up a node to define all inputs required for the preprocessing workflow
    """

    inputnode = pe.Node(interface=util.IdentityInterface(fields=['in_files',
                                                                 'fwhm',
                                                                 'mask_file']),
                        name='inputnode')

    """
    Smooth each run using SUSAN with the brightness threshold set to 75%
    of the median value for each run and a mask consituting the mean
    functional
    """

    smooth = pe.MapNode(interface=fsl.SUSAN(),
                        iterfield=['in_file', 'brightness_threshold','usans'],
                        name='smooth')

    """
    Determine the median value of the functional runs using the mask
    """


    if separate_masks:
        median = pe.MapNode(interface=fsl.ImageStats(op_string='-k %s -p 50'),
                            iterfield = ['in_file', 'mask_file'],
                            name='median')
    else:
        median = pe.MapNode(interface=fsl.ImageStats(op_string='-k %s -p 50'),
                            iterfield = ['in_file'],
                            name='median')
    susan_smooth.connect(inputnode, 'in_files', median, 'in_file')
    susan_smooth.connect(inputnode, 'mask_file', median, 'mask_file')

    """
    Mask the motion corrected functional runs with the dilated mask
    """

    if separate_masks:
        mask = pe.MapNode(interface=fsl.ImageMaths(suffix='_mask',
                                                   op_string='-mas'),
                          iterfield=['in_file', 'in_file2'],
                          name='mask')
    else:
        mask = pe.MapNode(interface=fsl.ImageMaths(suffix='_mask',
                                                   op_string='-mas'),
                          iterfield=['in_file'],
                          name='mask')
    susan_smooth.connect(inputnode, 'in_files', mask, 'in_file')
    susan_smooth.connect(inputnode, 'mask_file', mask, 'in_file2')

    """
    Determine the mean image from each functional run
    """

    meanfunc = pe.MapNode(interface=fsl.ImageMaths(op_string='-Tmean',
                                                    suffix='_mean'),
                           iterfield=['in_file'],
                           name='meanfunc2')
    susan_smooth.connect(mask, 'out_file', meanfunc, 'in_file')

    """
    Merge the median values with the mean functional images into a coupled list
    """

    merge = pe.Node(interface=util.Merge(2, axis='hstack'),
                        name='merge')
    susan_smooth.connect(meanfunc,'out_file', merge, 'in1')
    susan_smooth.connect(median,'out_stat', merge, 'in2')

    """
    Define a function to get the brightness threshold for SUSAN
    """
    susan_smooth.connect(inputnode, 'fwhm', smooth, 'fwhm')
    susan_smooth.connect(inputnode, 'in_files', smooth, 'in_file')
    susan_smooth.connect(median, ('out_stat', getbtthresh), smooth, 'brightness_threshold')
    susan_smooth.connect(merge, ('out', getusans), smooth, 'usans')

    outputnode = pe.Node(interface=util.IdentityInterface(fields=['smoothed_files']),
                    name='outputnode')

    susan_smooth.connect(smooth, 'smoothed_file', outputnode, 'smoothed_files')

    return susan_smooth
    
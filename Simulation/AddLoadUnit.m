function AddLoadUnit(sourceTable,thisPrefix,thisName,tStart,tEnd) %% Load needed loadData to memory
    if (ismember(strcat(thisPrefix,thisName), sourceTable.Properties.VariableNames))
        assignin('caller',strcat(thisPrefix,thisName),eval(strcat('sourceTable.',thisPrefix,thisName,'(',string(tStart),':',string(tEnd),')')));
    else
        assignin('caller',strcat(thisPrefix,thisName),0);
    end
end
